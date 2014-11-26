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
   {
   }

   size_t get_rate(size_t user_idx, size_t item_idx) const
   {
      return 3;
   }

private:

   void learn()
   {

   }

private:
   user_to_rates_t rates_;

   double mu;
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

      rates_map_t[values[0]][values[1]] = values[2];
   }

   fs::ifstream validation_data_input(validation_data_path);
   skip_csv_title(validation_data_input);
   while (validation_data_input)
   {
      auto values = read_csv_line(validation_data_input);
      if (values.empty())
         break;

      rates_map_t[values[0]][values[1]] = values[2];
   }


   return 0;
}
