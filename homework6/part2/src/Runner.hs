module Runner where

import           Backprop
import           Mnist
import           NeuralNet

import           Data.List
import           Data.Maybe
import           Numeric.LinearAlgebra

targets :: [[Double]]
targets =
    [
        [0.9, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1]
      , [0.1, 0.9, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1]
      , [0.1, 0.1, 0.9, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1]
      , [0.1, 0.1, 0.1, 0.9, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1]
      , [0.1, 0.1, 0.1, 0.1, 0.9, 0.1, 0.1, 0.1, 0.1, 0.1]
      , [0.1, 0.1, 0.1, 0.1, 0.1, 0.9, 0.1, 0.1, 0.1, 0.1]
      , [0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.9, 0.1, 0.1, 0.1]
      , [0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.9, 0.1, 0.1]
      , [0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.9, 0.1]
      , [0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.9]
    ]

interpret :: [Double] -> Int
interpret v = fromJust (elemIndex (maximum v) v)

isMatch :: (Eq a) => a -> a -> Int
isMatch x y = if x == y then 1 else 0

type LabelledImage = ([Double], Int)

trainOnePattern :: (NeuralNet n) => LabelledImage -> n -> n
trainOnePattern trainingData net = train net input target
  where input = fst trainingData
        digit = snd trainingData
        target = targets !! digit

trainWithAllPatterns :: (NeuralNet n) => n -> [LabelledImage] -> n
trainWithAllPatterns = foldr trainOnePattern

evalOnePattern :: (NeuralNet n) => n -> LabelledImage -> Int
evalOnePattern net trainingData = isMatch result target
  where input = fst trainingData
        target = snd trainingData
        rawResult = evaluate net input
        result = interpret rawResult

evalAllPatterns :: (NeuralNet n) => n -> [LabelledImage] -> [Int]
evalAllPatterns = map . evalOnePattern


readTrainingData :: IO [LabelledImage]
readTrainingData = do
  trainingLabels <- readLabels "mnist-data/train-labels-idx1-ubyte"
  trainingImages <- readImages "mnist-data/train-images-idx3-ubyte"
  return (zip (map normalisedData trainingImages) trainingLabels)

readTestData :: IO [LabelledImage]
readTestData = do
  testLabels <- readLabels "mnist-data/t10k-labels-idx1-ubyte"
  testImages <- readImages "mnist-data/t10k-images-idx3-ubyte"
  return (zip (map normalisedData testImages) testLabels)

