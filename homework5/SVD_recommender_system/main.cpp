#include <vector>
#include <iostream>
#include <sstream>
#include <cassert>
#include <unordered_map>

#include <boost/filesystem/path.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/fstream.hpp>

namespace fs = boost::filesystem;

void skip_csv_title(std::istream & in)
{
   in.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
}

std::vector<size_t> read_csv_line(std::istream & in)
{
   std::vector<size_t> result;

   std::string line;
   std::getline(in, line);
   std::istringstream ss(line);

   std::string value;
   while (std::getline(ss, value, ','))
   {
      result.push_back(stoull(value));
   }

   return result;
}

typedef std::unordered_map<size_t, size_t> item_to_rate_t;
typedef std::unordered_map<size_t, item_to_rate_t> user_to_rates_t;

struct solver_t
{
   solver_t(user_to_rates_t && rates)
      : rates_(std::move(rates))
      , mu_(0)
   {
      learn();
   }

   size_t get_rate(size_t user_idx, size_t item_idx)
   {
      initialize_params(user_idx, item_idx);
      return mu_ + users_base_[user_idx] + items_base_[item_idx]
            + dot_product(users_features_[user_idx], items_features_[item_idx]);
   }

private:
   void initialize_params(size_t user_idx, size_t item_idx)
   {
      if (!users_base_.count(user_idx))
         users_base_[user_idx] = 0;

      if (!items_base_.count(item_idx))
         items_base_[item_idx] = 0;

      if (!users_features_.count(user_idx))
         users_features_[user_idx] = features_t(features_count, 0);

      if (!items_features_.count(item_idx))
         items_features_[item_idx] = features_t(features_count, 0);
   }

   void learn()
   {
      static constexpr double eps = 1e-4;
      static constexpr double step = 1e-2;
      static constexpr double l1 = 1e-1;
      static constexpr double l2 = 1e-1;

      double old_rmse = std::numeric_limits<double>::max();
      while (true)
      {
         double error_squares = 0;
         size_t total_records = 0;
         for (auto const & user : rates_)
         {
            for (auto const & item : user.second)
            {
               ++total_records;
               auto user_idx = user.first;
               auto item_idx = item.first;
               initialize_params(user_idx, item_idx);
               auto calculated_rate = get_rate(user_idx, item_idx);
               double error = double(item.second) - calculated_rate;
               error_squares += error * error;

               mu_ += error * step;
               users_base_[user_idx] += step * (error - users_base_[user_idx] * l1);
               items_base_[item_idx] += step * (error - items_base_[item_idx] * l1);
               auto & user_features = users_features_[user_idx];
               auto & item_features = items_features_[item_idx];
               for (size_t i = 0; i != features_count; ++i)
               {
                  auto user_feature = user_features[i];
                  auto item_feature = item_features[i];
                  user_features[i] += step * (error * item_feature - l2 * user_feature);
                  item_features[i] += step * (error * user_feature - l2 * item_feature);
               }
            }
         }

         double rmse = std::sqrt(error_squares / total_records);

         std::clog << "Rmse: " << rmse << ", diff: " << old_rmse - rmse << std::endl;
         if (old_rmse - rmse < eps)
         {
            break;
         }
         else
         {
            old_rmse = rmse;
         }
      }
   }

   static constexpr size_t features_count = 10;
   typedef std::vector<double> features_t;

   double dot_product(features_t const & a, features_t const & b)
   {
      double res = 0;
      for (size_t i = 0; i != features_count; ++i)
      {
         res += a[i] * b[i];
      }

      return res;
   }

private:
   user_to_rates_t rates_;

   double mu_;
   std::unordered_map<size_t, double> users_base_;
   std::unordered_map<size_t, double> items_base_;

   std::unordered_map<size_t, features_t> users_features_;
   std::unordered_map<size_t, features_t> items_features_;
};

int main(int argc, char ** argv)
{
   fs::path data_dir = fs::current_path();

   if (argc == 2)
      data_dir = argv[1];

   fs::path train_data_path = data_dir / "train.csv";
   fs::path validation_data_path = data_dir / "validation.csv";
   fs::path test_ids_path = data_dir / "test-ids.csv";

   if (!fs::exists(train_data_path) || !fs::exists(validation_data_path) || !fs::exists(test_ids_path))
   {
      std::cerr << "Couldn't find all input files" << std::endl;
      return 1;
   }

   user_to_rates_t rates_map_t;

   fs::ifstream train_data_input(train_data_path);
   skip_csv_title(train_data_input);
   while (train_data_input)
   {
      auto values = read_csv_line(train_data_input);
      if (values.empty())
         break;

      assert(values.size() == 3);
      rates_map_t[values[0]][values[1]] = values[2];
   }

   fs::ifstream validation_data_input(validation_data_path);
   skip_csv_title(validation_data_input);
   while (validation_data_input)
   {
      auto values = read_csv_line(validation_data_input);
      if (values.empty())
         break;

      assert(values.size() == 3);
      rates_map_t[values[0]][values[1]] = values[2];
   }

   solver_t solver(std::move(rates_map_t));

   fs::ofstream output(data_dir / "submission.csv");
   output << "id,rating" << std::endl;
   fs::ifstream test_ids_input(test_ids_path);
   skip_csv_title(test_ids_input);
   while (test_ids_input)
   {
      auto values = read_csv_line(test_ids_input);
      if (values.empty())
         break;

      assert(values.size() == 3);
      size_t id = values[0];
      auto rate = solver.get_rate(values[1], values[2]);
      output << id << "," << rate << std::endl;
   }

   return 0;
}
