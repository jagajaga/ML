#include <vector>
#include <iostream>
#include <fstream>
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
}

int main(int argc, char ** argv)
{
   std::ifstream in("prices.txt");
   if (argc == 2) {
       in.open(argv[1], std::ifstream::in);
   }
   if (in.fail())
   {
      std::cerr << "Couldn't open input file" << std::endl;
      return 1;
   }

   std::vector<flat_t> flats;
   while (!in.eof())
   {
      flat_t flat;
      in >> flat.square >> flat.rooms_count >> flat.price;
      flats.push_back(flat);
   }

   size_t n = flats.size();
   matrix_t X(n, 3);
   for (size_t i = 0; i != n; ++i)
   {
      X(i, 0) = 1;
      X(i, 1) = flats[i].square;
      X(i, 2) = flats[i].rooms_count;
   }

   matrix_t Y(n, 1);
   for (size_t i = 0; i != n; ++i)
   {
      Y(i, 0) = flats[i].price;
   }

   auto X_transposed = boost::numeric::ublas::trans(X);
   matrix_t product(3, 3);
   boost::numeric::ublas::axpy_prod(X_transposed, X, product, true);
   matrix_t inverse(3, 3);
   invert_matrix(product, inverse);
   matrix_t product2(3, 3);
   boost::numeric::ublas::axpy_prod(inverse, X_transposed, product2, true);

   std::cerr << X << std::endl;
   std::cerr << Y << std::endl;
   std::cerr << product << std::endl;
   std::cerr << inverse << std::endl;

   return 0;
}
