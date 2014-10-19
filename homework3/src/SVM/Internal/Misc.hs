module SVM.Internal.Misc 
       
       where

import qualified Data.Vector.Generic as G
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Data.Array.Repa 


{-# INLINE atUV #-}     
atUV :: UV.Unbox a => UV.Vector a -> Int -> a
atUV = UV.unsafeIndex    
            
{-# INLINE atV #-}     
atV :: V.Vector a -> Int -> a
atV = V.unsafeIndex    

{-# INLINE atG #-}     
atG :: G.Vector v a => v a -> Int -> a
atG = G.unsafeIndex    

{-# INLINE atM #-}
atM :: (Source r e,Shape sh) => Array r sh e -> sh -> e
atM = unsafeIndex

{-# INLINE splitEvery #-}
splitEvery :: Int -> [a] -> [[a]]
splitEvery n [] = []
splitEvery n xs = take n xs : (splitEvery n $ drop n xs)
