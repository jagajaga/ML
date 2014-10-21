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


kernel2 :: X -> X -> Double
kernel2 a b = (dotB a b + 1.0) ^ 2

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

makeQ f shuffled len = matrix len len (\(i, j) -> let y1 = y $ shuffled !! i
                                                      y2 = y $ shuffled !! j
                                                      scalar = kernel2 (x $ shuffled !! i) (x $ shuffled !! j) in
                                                      fromIntegral (y1 * y2) * scalar)

a x_ lambdas ys xs w0 f = signum $ (\ann -> ann - w0) $ sum $ zipWith3 (\lam xs ys -> lam * (fromIntegral ys) * (f xs x_)) lambdas xs ys

part filename scalarMult = do
    contents <- readFile filename
    let arr =  map (\[a, b, c] -> Data [(read a :: Double), (read b :: Double)] ((\a -> if a == 0 then -1 else 1) $ read c :: Int)) $ map words $ lines contents
    shuffled <- shuffle arr
    let len = round ((fromIntegral $ length shuffled) * 0.8)
    let q = makeQ scalarMult shuffled len
    let yuv = UV.fromList $ map (\(Data _ a) -> a) shuffled
    let justY = map (\(Data _ a) -> a) shuffled
    let justX = map (\(Data b _) -> b) shuffled
    let qq = RE.fromListUnboxed (RE.ix2 len len) $ toList q
    let result@(Si w0 lambdas) = smoC 10 10 yuv qq
    let listLambdas = UV.toList lambdas
    let w@[w1, w2] = foldl (\ [c1, d1] [a1, b1] -> [a1 + c1, b1 + d1]) [0.0, 0.0] (zipWith3 (\[a, b] c d -> [a * (fromIntegral c) * d, b * (fromIntegral c) * d]) justX justY $ UV.toList lambdas)
    let i = (\((a, _)) -> a) $ head $ filter (\(a, b) -> b /= 0) $ zip [0..] listLambdas
    let w00 = (scalarMult w (justX !! i)) - (fromIntegral $ justY !! i)
    let x_ = (x (arr !! (len + 1)))
    let testingData = map (\xDot -> round $ a (x xDot) listLambdas justY justX w00 scalarMult) (drop len arr)
    let answer = foldl (\ acc nn -> if nn == True then acc + 1 else acc + 0) 0 $ zipWith (\gg hh -> if gg == (y hh) then True else False) testingData (drop len arr)
    print $ (fromIntegral answer) / (fromIntegral $ length (drop len arr))
    print $ "k"
    print $ (- (w1 / w2 ))
    print $ "b"
    print $ w00 / w2


main :: IO ()
main = do
    part "data/LinearDataset" dotB
    part "data/data-set.txt" kernel2
