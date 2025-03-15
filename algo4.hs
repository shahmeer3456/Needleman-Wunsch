module AntiDiagonalParallelAlignment where

import Data.Array
import Control.Parallel.Strategies

-- Scoring and gap penalty
score :: Char -> Char -> Int
score x y = if x == y then 1 else -1

gapPenalty :: Int
gapPenalty = -1

needlemanWunschAntiDiagonal :: String -> String -> Array (Int, Int) Int
needlemanWunschAntiDiagonal xs ys = dp
  where
    n = length xs
    m = length ys
    xsArr = listArray (1, n) xs
    ysArr = listArray (1, m) ys
    boundsDP = ((0,0), (n, m))
    
    initialDP = array boundsDP [ ((i,j), base i j) | i <- [0..n], j <- [0..m] ]
      where
        base i 0 = gapPenalty * i
        base 0 j = gapPenalty * j
        base _ _ = minBound :: Int
    
    updateDP dp k = dp // updates
      where
        indices = [ (i,j) | i <- [1..n], j <- [1..m], i + j == k ]
        updates = parMap rdeepseq (\(i,j) -> ((i,j), cell dp i j)) indices
        
        cell dp i j = maximum
          [ dp!(i-1, j-1) + score (xsArr!i) (ysArr!j)
          , dp!(i-1, j)   + gapPenalty
          , dp!(i, j-1)   + gapPenalty ]
    
    dp = foldl updateDP initialDP [2 .. (n+m)]

-- Print DP table
printDPTable :: String -> String -> IO ()
printDPTable xs ys = do
  let dp = needlemanWunschAntiDiagonal xs ys
      ((iMin, jMin), (iMax, jMax)) = bounds dp
  
  putStrLn "DP Table:"
  putStr "      "
  mapM_ (\j -> printf "%4d " j) [jMin..jMax]
  putStrLn ""
  
  forM_ [iMin..iMax] $ \i -> do
    printf "%4d " i
    mapM_ (\j -> printf "%4d " (dp ! (i, j))) [jMin..jMax]
    putStrLn ""

-- Example usage
main :: IO ()
main = printDPTable "AGCTGAC" "AGCTTGC"