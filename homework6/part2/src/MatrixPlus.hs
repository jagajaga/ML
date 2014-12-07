module MatrixPlus where

import           Numeric.LinearAlgebra as N

mapMatrix :: (Field a) => (a -> a) -> Matrix a -> Matrix a
mapMatrix f x = (r N.><c) y
  where x' = toList (flatten x)
        y = map f x'
        r = rows x
        c = cols x

type ColumnVector a = Matrix a

listToColumnVector :: (Ord a, Field a) => [a] -> ColumnVector a
listToColumnVector x = (len N.><1 ) x
    where len = length x

columnVectorToList :: (Ord a, Field a) => ColumnVector a -> [a]
columnVectorToList = toList . flatten
