{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes   #-}
module SVM.Types


       where
import           Control.DeepSeq
import           Data.Array.Repa
import qualified Data.IntMap         as M
import qualified Data.Text           as T
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as UV

data KernelPara = Linear
                | Poly    {-# UNPACK #-} !Int
                          {-# UNPACK #-} !Double -- ^ gamma
                          {-# UNPACK #-} !Double -- ^ coef0
                | RBF     {-# UNPACK #-} !Double -- ^ gamma
                | Sigmoid {-# UNPACK #-} !Double -- ^ gamma
                          {-# UNPACK #-} !Double -- ^ coef0
                  deriving (Eq)

type Matrix a = Array U DIM2 a
type Sample a = RealFloat a => V.Vector (UV.Vector a)
type Label = UV.Vector Int
type Indexes = UV.Vector Int


data DataSet a = DataSet {
    labelText :: !(Maybe (M.IntMap T.Text)) -- ^ Text label
   ,labels    :: !Label                        -- ^ classification categories y = y1,...,yn
                                           -- ^ yi in [-1,1] for 2-class
                                           -- ^ yi in [1..nClass] for multi-class
   ,samples   :: !(Sample a)                  -- ^ x = n * l Matrix
   ,idxSlice  :: !(M.IntMap (V.Vector Int))  -- ^ Indexes for each class, key = y_i
  }

data SVM a = SVM {
   para    :: !SVMPara
  ,dataset :: !(DataSet a)
  ,matrixK :: !(Matrix a)
  }


data Strategy = OVA  -- ^ one vs all
              | OVO  -- ^ one vs one
              | DAG

data SVMPara = SVMPara {
   kernelPara :: !KernelPara
  ,cost       :: {-# UNPACK #-} !Double
  ,weight     :: !(M.IntMap Double)
  ,strategy   :: !Strategy
--,probEst :: {-# UNPACK #-} !Bool
  }


type Coefs = UV.Vector Double
data SVCoef = SVCoef {-# UNPACK #-}!Double
              !Indexes
              !Coefs

data SolutionInfo = Si {-# UNPACK #-} !Double
                    !(UV.Vector Double) deriving Show

data Model a = Model !(SVM a) !(M.IntMap SVCoef)

setGamma :: SVMPara -> Double -> SVMPara
setGamma !svmPara !gamma =
  case kernelPara svmPara of
    Poly i g c  -> svmPara {kernelPara=Poly i gamma c}
    RBF  g      -> svmPara {kernelPara=RBF gamma}
    Sigmoid g c -> svmPara {kernelPara=Sigmoid gamma c}
    _           -> svmPara

setCost :: SVMPara -> Double -> SVMPara
setCost !svmPara !c = svmPara {cost=c}

setWeight :: SVMPara -> M.IntMap Double -> SVMPara
setWeight !svmPara !w = svmPara {weight=w}

instance Show (DataSet a) where
  show dataSet = ""
instance NFData SVCoef where
  rnf (SVCoef r idxs coefs) = r `seq` idxs `seq` coefs `seq` ()


