module ColumnParallelAlignment where

import Data.Array
import Control.Parallel.Strategies
import Text.Printf

-- Scoring function
-- This function returns a score based on the comparison of two characters.
score :: Char -> Char -> Int
score x y = if x == y then 1 else -1

-- Gap penalty
-- This constant defines the penalty for introducing a gap in the alignment.
gapPenalty :: Int
gapPenalty = -1

-- Needleman-Wunsch alignment algorithm with column-parallel computation
-- This function performs the Needleman-Wunsch algorithm to align two sequences.
needlemanWunschColumn :: String -> String -> Array (Int, Int) Int
needlemanWunschColumn xs ys = dp
  where
    n = length xs
    m = length ys
    xsArr = listArray (1, n) xs
    ysArr = listArray (1, m) ys
    boundsDP = ((0,0), (n, m))
    
    -- Initialize the first column of the dynamic programming table
    col0 = listArray (0, n) [ gapPenalty * i | i <- [0..n] ]
    
    -- Compute all columns using the previous column
    columns = scanl computeNextColumn col0 [1..m]
    
    -- Construct the final dynamic programming table
    dp = array boundsDP [ ((i, j), (columns !! j) ! i) | i <- [0..n], j <- [0..m] ]
    
    -- Function to compute the next column in the dynamic programming table
    computeNextColumn prevCol j = listArray (0, n) currentCol
      where
        currentCol = [ cell i j prevCol | i <- [0..n] ] `using` parList rseq
        
        -- Base case for the first row
        cell 0 j _ = gapPenalty * j
        -- Compute the value for cell (i, j)
        cell i j prev =
          maximum [ 
            prevCol ! (i-1) + score (xsArr ! i) (ysArr ! j),
            prevCol ! i + gapPenalty,
            if i > 0 then (currentCol !! (i-1)) + gapPenalty else gapPenalty
          ]

-- Format and print the matrix
-- This function formats and prints the alignment matrix for the two sequences.
formatAlignmentMatrix :: String -> String -> Array (Int, Int) Int -> IO ()
formatAlignmentMatrix seq1 seq2 matrix = do
    putStrLn $ "Sequence 1: " ++ seq1
    putStrLn $ "Sequence 2: " ++ seq2
    putStrLn ""
    
    -- Print header row
    printf "    -  "
    mapM_ (\c -> printf "%3c " c) ('-':seq1)
    putStrLn ""
    
    -- Print each row of the matrix
    let (_, (rows, cols)) = bounds matrix
    forM_ [0..rows] $ \i -> do
        -- Print row label
        let rowLabel = if i == 0 then "-" else [seq2 !! (i-1)]
        printf "%s   " rowLabel
        
        -- Print row values
        forM_ [0..cols] $ \j -> do
            let val = matrix ! (i, j)
            printf "%3d " val
        putStrLn ""

-- Main function to demonstrate the alignment
-- This function runs the alignment algorithm on two example sequences.
main :: IO ()
main = do
    let seq1 = "AGCTGAC"
        seq2 = "AGCTTGC"
    let alignmentMatrix = needlemanWunschColumn seq1 seq2
    formatAlignmentMatrix seq1 seq2 alignmentMatrix
