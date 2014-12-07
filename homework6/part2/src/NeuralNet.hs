module NeuralNet where

import           MatrixPlus

import           Numeric.LinearAlgebra as N

class NeuralNet n where
  evaluate :: n -> [Double] -> [Double]
  train    :: n -> [Double] -> [Double] -> n
