module Main where
import           Control.Monad
import           Data.Array.IO
import qualified Data.Array.Repa       as RE
import           Data.Array.Repa.Index
import           Data.List
import           Data.Matrix
import qualified Data.Vector.Unboxed   as UV
import           SVM.Internal.SMO
import           SVM.Types
import           System.IO
import           System.Random

type X = [Double]

type Y = Int

data Data = Data {x :: X, y :: Y} deriving Show

dotB :: X -> X -> Double
dotB a b = sum (zipWith (*) a b)



-- | Randomly shuffle a list
--   /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n =  newListArray (1,n)

makeQ shuffled len = matrix len len (\(i, j) -> let y1 = y $ shuffled !! i
                                                    y2 = y $ shuffled !! j
                                                    scalar = dotB (x $ shuffled !! i) (x $ shuffled !! j) in
                                                    fromIntegral (y1 * y2) * scalar)

main :: IO ()
main = do
    contents <- readFile "data/LinearDataset"
    let arr =  map (\[a, b, c] -> Data [(read a :: Double), (read b :: Double)] ((\a -> if a == 0 then -1 else 1) $ read c :: Int)) $ map words $ lines contents
    shuffled <- shuffle arr
    let len = round ((fromIntegral $ length shuffled) * 0.8)
    let q = makeQ shuffled len
    let yuv = UV.fromList $ map (\(Data _ a) -> a) shuffled
    let justY = map (\(Data _ a) -> a) shuffled
    let justX = map (\(Data b _) -> b) shuffled
    let qq = RE.fromListUnboxed (RE.ix2 len len) $ toList q
    let result@(Si w0 lambdas) = smoC 10 10 yuv qq
    let listLambdas = UV.toList lambdas
    let w = foldl (\ [c1, d1] [a1, b1] -> [a1 + c1, b1 + d1]) [0.0, 0.0] (zipWith3 (\[a, b] c d -> [a * (fromIntegral c) * d, b * (fromIntegral c) * d]) justX justY $ UV.toList lambdas)
    let i = (\((a, _)) -> a) $ head $ filter (\(a, b) -> b /= 0) $ zip [0..] listLambdas 
    let w00 = (dotB w (justX !! i)) - (fromIntegral $ justY !! i) 
    print "our: "
    print w00
    print "function's: "
    print w0

