#include <vector>
#include <iostream>
#include <fstream>
#include <array>

#include <boost/optional.hpp>

#include <boost/range/any_range.hpp>

#include <boost/numeric/ublas/matrix.hpp>
#include <boost/numeric/ublas/operation.hpp>
#include <boost/numeric/ublas/lu.hpp>
#include <boost/numeric/ublas/io.hpp>

namespace
{
   typedef boost::numeric::ublas::matrix<double> matrix_t;
   typedef boost::numeric::ublas::identity_matrix<double> identity_matrix_t;

   /* Matrix inversion routine.
   Uses lu_factorize and lu_substitute in uBLAS to invert a matrix */
   bool invert_matrix(matrix_t const & input, matrix_t & inverse)
   {
     typedef boost::numeric::ublas::permutation_matrix<std::size_t> pmatrix;

     // create a working copy of the input
     matrix_t A(input);

     // create a permutation matrix for the LU-factorization
     pmatrix pm(A.size1());

     // perform LU-factorization
     int res = lu_factorize(A, pm);
     if (res != 0)
        return false;

     // create identity matrix of "inverse"
     inverse.assign(identity_matrix_t(A.size1()));

     // backsubstitute to get the inverse
     boost::numeric::ublas::lu_substitute(A, pm, inverse);

     return true;
   }

   struct flat_t
   {
      unsigned square;
      unsigned rooms_count;
      unsigned price;
   };

   typedef boost::any_range<flat_t const, boost::forward_traversal_tag, flat_t const &, std::ptrdiff_t> flat_range_t;

   struct flat_price_calculator_t
   {
      flat_price_calculator_t(flat_range_t const & learning_data, size_t limit)
      {
         size_t learning_n = std::min(boost::size(learning_data), limit);
         matrix_t X(learning_n, 3);
         for (size_t i = 0; i != learning_n; ++i)
         {
            X(i, 0) = 1;
            X(i, 1) = flats[i].square;
            X(i, 2) = flats[i].rooms_count;
         }

         matrix_t Y(learning_n, 1);
         for (size_t i = 0; i != learning_n; ++i)
         {
            Y(i, 0) = flats[i].price;
         }

         auto X_transposed = boost::numeric::ublas::trans(X);
         matrix_t product(3, 3);
         boost::numeric::ublas::axpy_prod(X_transposed, X, product, true);
         matrix_t inverse(3, 3);
         invert_matrix(product, inverse);
         matrix_t product2(3, learning_n);
         boost::numeric::ublas::axpy_prod(inverse, X_transposed, product2, true);
         matrix_t betas(3, 1);
         boost::numeric::ublas::axpy_prod(product2, Y, betas, true);

      }

      unsigned operator()(unsigned square, unsigned rooms_cout) const
      {
         return 0;
      }

   private:
      std::array<double, 3> betas_;
   };

   double rmse(flat_range_t const & flats, flat_price_calculator_t const & calculator)
   {
      unsigned sum = 0;
      for (auto const & flat : flats)
      {
         auto real_price = flat.price;
         auto calculated_price = calculator(flat.square, flat.rooms_count);
         sum += ((real_price - calculated_price) * (real_price - calculated_price));
      }

      return std::sqrt(double(sum) / boost::size(flats));
   }
}

int main(int argc, char ** argv)
{
   boost::optional<std::ifstream> in;
   if (argc == 2)
   {
      in = boost::in_place(argv[1]);
   }
   else
   {
      in = boost::in_place("prices.txt");
   }

   if (in->fail())
   {
      std::cerr << "Couldn't open input file" << std::endl;
      return 1;
   }

   std::vector<flat_t> flats;
   while (!in->eof())
   {
      flat_t flat;
      *in >> flat.square >> flat.rooms_count >> flat.price;
      flats.push_back(flat);
   }

   size_t learning_n = flats.size() * 4 / 5;
   flat_price_calculator_t flat_price_calculator(flats, learning_n);

   std::cout << rmse(flats, flat_price_calculator); // FIXME
   return 0;
}
