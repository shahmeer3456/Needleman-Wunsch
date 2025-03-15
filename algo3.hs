{-# LANGUAGE ParallelListComp #-}
module RowParallelAlignment where

import Data.Array
import Control.Parallel.Strategies
import System.IO

-- Scoring function and gap penalty
score :: Char -> Char -> Int
score x y = if x == y then 1 else -1

gapPenalty :: Int
gapPenalty = -1

-- Main Needleman-Wunsch implementation with row-wise parallelism
needlemanWunschRow :: String -> String -> Array (Int, Int) Int
needlemanWunschRow xs ys = dp
  where
    n = length xs
    m = length ys
    xsArr = listArray (1, n) xs
    ysArr = listArray (1, m) ys
    
    boundsDP = ((0,0), (n, m))
    
    -- Initialize first row
    row0 = listArray (0, m) [gapPenalty * j | j <- [0..m]]
    
    -- Compute subsequent rows
    rows = scanl computeNextRow row0 [1..n]
    
    -- Final DP table
    dp = array boundsDP [((i,j), (rows !! i) ! j) | i <- [0..n], j <- [0..m]]
    
    computeNextRow prevRow i = listArray (0, m) currentRow
      where
        currentRow = [ cell j prevRow i | j <- [0..m] ] `using` parList rseq
        
        cell 0 _ _ = gapPenalty * i
        cell j prevRow i = maximum
          [ prevRow ! (j-1) + score (xsArr ! i) (ysArr ! j)  -- Diagonal (match/mismatch)
          , prevRow ! j + gapPenalty                         -- Above (gap in seq1)
          , if j > 0 then (currentRow !! (j-1)) + gapPenalty -- Left (gap in seq2)
                     else gapPenalty
          ]

-- Pretty-print DP table
printDPTable :: String -> String -> IO ()
printDPTable xs ys = do
  let dp = needlemanWunschRow xs ys
      ((iMin, jMin), (iMax, jMax)) = bounds dp
  
  putStrLn "\nDynamic Programming Table:"
  putStr "    "
  mapM_ (\j -> printf "%4d " j) [jMin..jMax]
  putStrLn ""
  
  forM_ [iMin..iMax] $ \i -> do
    printf "%2d " i
    mapM_ (\j -> printf "%4d " (dp ! (i, j))) [jMin..jMax]
    putStrLn ""

-- Example usage with test sequences
main :: IO ()
main = do
  let seq1 = "AGCTGAC"
      seq2 = "AGCTTGC"
  
  putStrLn "Sequence 1: AGCTGAC"
  putStrLn "Sequence 2: AGCTTGC"
  printDPTable seq1 seq2
  
  let dp = needlemanWunschRow seq1 seq2
      (n, m) = snd (bounds dp)
  
  putStrLn $ "\nOptimal Alignment Score: " ++ show (dp ! (n, m))