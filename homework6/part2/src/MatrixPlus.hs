module MatrixPlus where

import           Numeric.LinearAlgebra as N

dummyMatrix :: (Field a) => Matrix a
dummyMatrix = (0 N.><0) []

mapMatrix :: (Field a)
  => (a -> a)
  -> Matrix a
  -> Matrix a
mapMatrix f x = (r N.><c) y
  where x' = toList (flatten x)
        y = map f x'
        r = rows x
        c = cols x

zipMatricesWith :: (Field a)
  => (a -> a -> a)
  -> Matrix a
  -> Matrix a
  -> Matrix a
zipMatricesWith f x y = (r N.><c) z
  where x' = toList (flatten x)
        y' = toList (flatten y)
        z = zipWith f x' y'
        r = rows x
        c = cols x

hadamardProduct :: (Field a)
  => Matrix a
  -> Matrix a
  -> Matrix a
hadamardProduct = zipMatricesWith (*)

average :: (Field a)
  => Matrix a
  -> a
average m = sum ms / fromIntegral (length ms)
  where ms = toList (flatten m)

magnitude :: (Field a, Floating a)
  => Matrix a
  -> a
magnitude x =
  if cols x == 1
  then sqrt (sum xsxs)
  else error "not a column vector"
    where xs = toList (flatten x)
          xsxs = zipWith (*) xs xs

pseudoMagnitude :: (Field a, Floating a)
  => Matrix a
  -> a
pseudoMagnitude m = sqrt (sum msms)
    where ms = toList (flatten m)
          msms = zipWith (*) ms ms

type ColumnVector a = Matrix a

listToColumnVector :: (Ord a, Field a)
    => [a]
    -> ColumnVector a
listToColumnVector x = (len N.><1 ) x
    where len = length x

columnVectorToList :: (Ord a, Field a)
    => ColumnVector a
    -> [a]
columnVectorToList = toList . flatten
