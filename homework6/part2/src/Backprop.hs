module Backprop
  (BackpropNet, buildBackpropNet, tanhAS, identityAS)
    where

import           MatrixPlus            as P
import           NeuralNet

import           Control.Exception
import           Numeric.LinearAlgebra as N

data Layer = Layer {
      lW  :: Matrix Double,
      lAS :: ActivationSpec
    }

instance Show Layer where
    show layer = "w=" ++ show (lW layer) ++ ", activation spec=" ++ show (lAS layer)

inputWidth :: Layer -> Int
inputWidth = cols . lW

data PropagatedLayer = PropagatedLayer { pIn  :: ColumnVector Double,
                                         pOut :: ColumnVector Double,
                                         pF'a :: ColumnVector Double,
                                         pW   :: Matrix Double,
                                         pAS  :: ActivationSpec
                                       }
                                       | PropagatedSensorLayer { pOut :: ColumnVector Double }

instance Show PropagatedLayer where
    show (PropagatedLayer x y f'a w s) =
        "in=" ++ show x
        ++ ", out=" ++ show y
        ++ ", f'(a)=" ++ show f'a
        ++ ", w=" ++ show w
        ++ ", " ++ show s
    show (PropagatedSensorLayer x) = "out=" ++ show x

propagate :: PropagatedLayer -> Layer -> PropagatedLayer
propagate layerJ layerK = PropagatedLayer { pIn = x,
          pOut = y,
          pF'a = f'a,
          pW   = w,
          pAS  = lAS layerK
        }
  where x   = pOut layerJ
        w   = lW layerK
        a   = w <> x
        f   = asF ( lAS layerK )
        y   = P.mapMatrix f a
        f'  = asF' ( lAS layerK )
        f'a = P.mapMatrix f' a

data BackpropagatedLayer = BackpropagatedLayer {
      bpDazzle  :: ColumnVector Double
    , bpErrGrad :: ColumnVector Double
    , bpF'a     :: ColumnVector Double
    , bpIn      :: ColumnVector Double
    , bpOut     :: ColumnVector Double
    , bpW       :: Matrix Double
    , bpAS      :: ActivationSpec
    }

instance Show BackpropagatedLayer where
    show layer =
        "dazzle=" ++ show (bpDazzle layer)
        ++ ", grad=" ++ show (bpErrGrad layer)
        ++ ", in=" ++ show (bpIn layer)
        ++ ", out=" ++ show (bpOut layer)
        ++ ", w=" ++ show (bpW layer)
        ++ ", activationFunction=?, activationFunction'=?"

backpropagateFinalLayer :: PropagatedLayer -> ColumnVector Double -> BackpropagatedLayer
backpropagateFinalLayer l t = BackpropagatedLayer { bpDazzle = dazzle,
      bpErrGrad = errorGrad dazzle f'a (pIn l),
      bpF'a     = pF'a l,
      bpIn      = pIn l,
      bpOut     = pOut l,
      bpW       = pW l,
      bpAS      = pAS l
    }
    where dazzle = pOut l - t
          f'a    = pF'a l

errorGrad :: ColumnVector Double -> ColumnVector Double -> ColumnVector Double
    -> ColumnVector Double
errorGrad dazzle f'a input = (dazzle * f'a) <> trans input

backpropagate :: PropagatedLayer -> BackpropagatedLayer -> BackpropagatedLayer
backpropagate layerJ layerK = BackpropagatedLayer { bpDazzle = dazzleJ,
      bpErrGrad = errorGrad dazzleJ f'aJ (pIn layerJ),
      bpF'a     = pF'a layerJ,
      bpIn      = pIn layerJ,
      bpOut     = pOut layerJ,
      bpW       = pW layerJ,
      bpAS      = pAS layerJ
    }
    where dazzleJ = wKT <> (dazzleK * f'aK)
          dazzleK = bpDazzle layerK
          wKT     = trans ( bpW layerK )
          f'aK    = bpF'a layerK
          f'aJ    = pF'a layerJ

update :: Double -> BackpropagatedLayer -> Layer
update rate layer = Layer { lW  = wNew, lAS = bpAS layer }
    where wOld = bpW layer
          delW = rate `scale` bpErrGrad layer
          wNew = wOld - delW

data BackpropNet = BackpropNet { layers       :: [Layer]
                               , learningRate :: Double
                               } deriving Show

buildBackpropNet :: Double -> [Matrix Double] -> ActivationSpec -> BackpropNet
buildBackpropNet lr ws s = BackpropNet { layers = ls, learningRate = lr }
  where checkedWeights = scanl1 checkDimensions ws
        ls             = map buildLayer checkedWeights
        buildLayer w   = Layer { lW = w, lAS = s }

checkDimensions :: Matrix Double -> Matrix Double -> Matrix Double
checkDimensions w1 w2 = if rows w1 == cols w2 then w2 else error "Inconsistent dimensions in weight matrix"

propagateNet :: ColumnVector Double -> BackpropNet -> [PropagatedLayer]
propagateNet input net = tail calcs
  where calcs           = scanl propagate layer0 (layers net)
        layer0          = PropagatedSensorLayer{ pOut = validatedInputs }
        validatedInputs = validateInput net input

validateInput :: BackpropNet -> ColumnVector Double -> ColumnVector Double
validateInput net = validateInputValues . validateInputDimensions net

validateInputDimensions :: BackpropNet -> ColumnVector Double -> ColumnVector Double
validateInputDimensions net input =
  if got == expected
       then input
       else error ("Input pattern has " ++ show got ++ " bits, but " ++ show expected ++ " were expected")
           where got      = rows input
                 expected = inputWidth (head (layers net))

validateInputValues :: ColumnVector Double -> ColumnVector Double
validateInputValues input =
  if (min >= 0) && (max <= 1)
       then input
       else error "Input bits outside of range [0,1]"
       where min = minimum ns
             max = maximum ns
             ns  = toList ( flatten input )

backpropagateNet :: ColumnVector Double -> [PropagatedLayer] -> [BackpropagatedLayer]
backpropagateNet target layers = scanr backpropagate layerL hiddenLayers
  where hiddenLayers = init layers
        layerL       = backpropagateFinalLayer (last layers) target

instance NeuralNet BackpropNet where
  evaluate = evaluateBPN
  train    = trainBPN

evaluateBPN :: BackpropNet -> [Double] -> [Double]
evaluateBPN net input = columnVectorToList( pOut ( last calcs ))
  where calcs = propagateNet x net
        x     = listToColumnVector (1:input)

trainBPN :: BackpropNet -> [Double] -> [Double] -> BackpropNet
trainBPN net input target = BackpropNet { layers = newLayers, learningRate = rate }
  where newLayers            = map (update rate) backpropagatedLayers
        rate                 = learningRate net
        backpropagatedLayers = backpropagateNet (listToColumnVector target) propagatedLayers
        propagatedLayers     = propagateNet x net
        x                    = listToColumnVector (1:input)

data ActivationSpec = ActivationSpec { asF  :: Double -> Double
                                     , asF' :: Double -> Double
                                     , desc :: String
                                     }

instance Show ActivationSpec where
    show = desc

identityAS :: ActivationSpec
identityAS = ActivationSpec { asF = id
                            , asF' = const 1
                            , desc = "identity"
                            }

tanhAS :: ActivationSpec
tanhAS = ActivationSpec { asF = tanh
                        , asF' = tanh'
                        , desc = "tanh"
                        }

tanh' :: Double -> Double
tanh' x = 1 - (tanh x)^2
