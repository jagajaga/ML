#include <vector>
#include <iostream>
#include <fstream>

struct flat_t
{
   unsigned int square;
   unsigned int rooms_count;
   unsigned int price;
};

int main()
{
   std::ifstream in("prices.txt");
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

   return 0;
}
