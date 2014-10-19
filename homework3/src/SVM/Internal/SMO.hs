{-# LANGUAGE BangPatterns #-}
module SVM.Internal.SMO
       (
         smoC
       )
       where
import           Control.Monad
import           Control.Monad.ST.Strict
import           Data.Array.Repa
import           Data.List
import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Unboxed.Mutable as MV
import           SVM.Internal.Misc
import           SVM.Types

data PID = PID {-# UNPACK #-} !Int
               {-# UNPACK #-} !Double
           
data PII = PII {-# UNPACK #-} !Int           
               {-# UNPACK #-} !Int
           
data PIDD = PIDD {-# UNPACK #-} !Int
                 {-# UNPACK #-} !Double
                 {-# UNPACK #-} !Double
data PVDD = PVDD !(UV.Vector Double)            
                 {-# UNPACK #-} !Double
                 {-# UNPACK #-} !Double
            
data RhoTmp = R {-# UNPACK #-} !Int
              {-# UNPACK #-} !Double
              {-# UNPACK #-} !Double
              {-# UNPACK #-} !Double

-- | An SMO algorithm in Fan et al., JMLR 6(2005), p. 1889--1918
-- Solves:
-- >	min 0.5(\alpha^T Q \alpha) - e^T \alpha
-- >		y^T \alpha = \delta
-- >		y_i = +1 or -1
-- >		0 <= alpha_i <= Cp for y_i = 1
-- >		0 <= alpha_i <= Cn for y_i = -1
-- >            e is the vector of all ones
{-# INLINE smoC #-}
{-# SPECIALIZE smoC :: Double -> Double -> Label -> Matrix Double -> SolutionInfo #-}
{-# SPECIALIZE smoC :: Double -> Double -> Label -> Matrix Float -> SolutionInfo #-}
smoC :: (UV.Unbox a,RealFloat a) => 
        Double ->    -- ^ Cp for y_i = 1
        Double ->    -- ^ Cn for y_i = -1
        Label ->     -- ^ y
        Matrix a ->  -- ^ Q[i][j] = y[i]*y[j]*K[i][j]; K: kernel matrix
        SolutionInfo -- ^ rho and alpha
smoC !costP !costN !y !mQ = let l = UV.length y
                                vAlpha = UV.replicate l 0.0
                                vGradient = UV.replicate l (-1.0)
                            in go vAlpha vGradient 0
  where 
    !len = UV.length y
    eps=1e-3 :: Double
    tau=1e-12 :: Double
    !maxIter = len * 2   -- need more test 
    !vY =UV.map fromIntegral y
    {-# INLINE go #-}
    go :: UV.Vector Double -> UV.Vector Double -> Int -> SolutionInfo
    go !vA !vG !iter | iter < maxIter =
      case selectedPair of
        PII _ (-1) -> Si rho vA
        PII i j    -> 
          let !t_Yi = vY `atUV` i
              !t_Yj = vY `atUV` j
              !t_Gi = vG `atUV` i
              !t_Gj = vG `atUV` j
              !tmp  = mQ `atM` (Z:.i:.i) +
                      mQ `atM` (Z:.j:.j) -
                      2.0 * (realToFrac $ t_Yi * t_Yj) * 
                      mQ `atM` (Z:.i:.j)
              !a    = if tmp <= 0
                      then tau
                      else realToFrac tmp
              !b    = -(t_Yi * t_Gi) + t_Yj * t_Gj                          
              !(PVDD vA' oAi oAj) = runST $ do
                vmA <- UV.unsafeThaw vA
                oldAi <- MV.read vmA i
                oldAj <- MV.read vmA j
                let !sum_tmp = t_Yi * oldAi + t_Yj * oldAj
                    !t_Ai = oldAi + t_Yi * b / a
                    !cost_i = if y `atUV` i == 1
                              then costP
                              else costN
                MV.write vmA i t_Ai
                when (t_Ai > cost_i) $
                  MV.write vmA i cost_i
                when (t_Ai < 0) $  
                  MV.write vmA i 0.0
                  
                t_Ai' <- MV.read vmA i  
                let !t_Aj = t_Yj * (sum_tmp - t_Yi * t_Ai')
                    !cost_j = if y `atUV` j == 1
                              then costP
                              else costN
                MV.write  vmA j t_Aj
                when (t_Aj > cost_j) $
                  MV.write vmA j cost_j
                when (t_Aj < 0) $
                  MV.write vmA j 0
                t_Aj' <- MV.read vmA j
                MV.write vmA i $! t_Yi * (sum_tmp - t_Yj * t_Aj')
                vA_new <- UV.unsafeFreeze vmA
                return $! PVDD vA_new oldAi oldAj
              !deltaAi = vA' `atUV` i - oAi  
              !deltaAj = vA' `atUV` j - oAj
              !iter' = iter + 1
              !vG' = runST $ do
                vmG <- UV.unsafeThaw vG
                forM_ [0..len-1] $ \t -> do
                  t_G <- MV.read vmG t
                  MV.write vmG t $! t_G + (realToFrac (mQ `atM` (Z:.t:.i))) * deltaAi +
                    (realToFrac (mQ `atM` (Z:.t:.j))) * deltaAj
                UV.unsafeFreeze vmG
          in go vA' vG' iter'
             | otherwise = Si rho vA
      where                                    
        !rho = let !(R nf sf up low) = 
                     UV.ifoldl' (\(R nr_free sum_free ub lb) i g ->
                                  let !yG = vY `UV.unsafeIndex` i * g 
                                      !alpha = vA `UV.unsafeIndex` i
                                      !yL = y `UV.unsafeIndex` i
                                      !c = if y `UV.unsafeIndex` i == 1 
                                           then costP
                                           else costN                                  
                                  in if alpha >= c
                                     then if yL == (-1)
                                          then let !ub' = min ub yG
                                               in R nr_free sum_free ub' lb
                                          else let !lb' = max lb yG
                                               in R nr_free sum_free ub lb'
                                     else if alpha <= 0
                                          then if yL == 1
                                               then let !ub' = min ub yG
                                                    in R nr_free sum_free ub' lb
                                               else let !lb' = max lb yG
                                                    in R nr_free sum_free ub lb'
                                          else let !nr_free' = nr_free + 1
                                                   !sum_free' = sum_free + yG           
                                               in R nr_free' sum_free' ub lb
                                ) (R 0 0.0 (1.0/0) (-1.0/0)) vG
               in if nf > 0
                  then sf / fromIntegral nf
                  else (up+low)/2
        {-# INLINE selectedPair #-}
        selectedPair = 
          let !(PID i max_G) = foldl' (\p@(PID _ max_G') t->
                                     let 
                                       !t_G = vG `atUV` t
                                       !t_A = vA `atUV` t
                                       !t_y = y `atUV` t
                                       !g_Max = -(vY `atUV` t) * t_G
                                     in if ((t_y == 1 && t_A < costP) ||
                                            (t_y == (-1) && t_A > 0)) &&
                                           g_Max >= max_G'
                                        then PID t g_Max
                                        else p
                                   ) (PID (-1) (-1.0/0)) [0..len-1]
              !(PIDD j min_G _) = 
                foldl' (\p@(PIDD j' min_G' obj_min) t ->
                         let 
                           !t_y = y `atUV` t
                           !t_A = vA `atUV` t
                           !t_G = vG `atUV` t
                           !t_vY= vY `atUV` t
                           !tmp = t_vY * t_G
                         in if (t_y == 1 && t_A > 0) || 
                                (t_y == (-1) && t_A < costN)
                            then let !b = max_G + tmp
                                     !g_min = if (-tmp) <= min_G'
                                              then (-tmp)
                                              else min_G'
                                 in if b > 0
                                    then let !g = realToFrac (mQ `atM` (Z:.i:.i) + 
                                                  mQ `atM` (Z:.t:.t)) -
                                                  2.0 * (vY `atUV` i) * 
                                                  t_vY * (realToFrac (mQ `atM` (Z:.i:.t)))
                                             !a = if g <= 0 then tau else g
                                             !obj_diff_min = - (b*b) / a
                                         in if obj_diff_min <= obj_min
                                            then PIDD t g_min obj_diff_min
                                            else PIDD j' g_min obj_min
                                    else PIDD j' g_min obj_min
                            else p
                       ) (PIDD (-1) (1.0/0) (1.0/0)) [0..len-1]
          in if max_G - min_G < eps
             then PII (-1) (-1)
             else PII i j
    
