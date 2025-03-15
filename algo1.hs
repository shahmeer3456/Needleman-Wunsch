module SequentialAlignment where
import Data.Array

-- Scoring function: match = 1, mismatch = -1
score :: Char -> Char -> Int
score x y = if x == y then 1 else -1

-- Constant gap penalty
gapPenalty :: Int
gapPenalty = -1

-- Data type to store direction for traceback
data Direction = Diag | Up | Left | None deriving (Show, Eq)

-- Main Needleman-Wunsch alignment function
needlemanWunsch :: String -> String -> (Array (Int, Int) Int, Array (Int, Int) Direction)
needlemanWunsch xs ys = (dp, directions)
  where
    n = length xs
    m = length ys
    
    xsArr = listArray (1, n) xs
    ysArr = listArray (1, m) ys
    
    boundsDP = ((0, 0), (n, m))
    
    dp = array boundsDP [ ((i, j), fst $ cell i j) | i <- [0..n], j <- [0..m] ]
    directions = array boundsDP [ ((i, j), snd $ cell i j) | i <- [0..n], j <- [0..m] ]
    
    cell :: Int -> Int -> (Int, Direction)
    cell 0 0 = (0, None)
    cell i 0 = (gapPenalty * i, Up)
    cell 0 j = (gapPenalty * j, Left)
    cell i j = 
        let diagScore = dp!(i-1, j-1) + score (xsArr!i) (ysArr!j)
            upScore = dp!(i-1, j) + gapPenalty
            leftScore = dp!(i, j-1) + gapPenalty
            maxScore = maximum [diagScore, upScore, leftScore]
        in (maxScore, 
            if maxScore == diagScore then Diag
            else if maxScore == upScore then Up
            else Left)

-- Function to get the actual alignment using traceback
getAlignment :: String -> String -> (String, String)
getAlignment xs ys = 
    let (dp, dirs) = needlemanWunsch xs ys
        n = length xs
        m = length ys
        xsArr = listArray (1, n) xs
        ysArr = listArray (1, m) ys
        
        traceback :: Int -> Int -> (String, String) -> (String, String)
        traceback 0 0 acc = acc
        traceback i j (xAcc, yAcc) = 
            case dirs!(i,j) of
                Diag -> traceback (i-1) (j-1) (xsArr!i : xAcc, ysArr!j : yAcc)
                Up   -> traceback (i-1) j     (xsArr!i : xAcc, '-' : yAcc)
                Left -> traceback i (j-1)     ('-' : xAcc, ysArr!j : yAcc)
                None -> (xAcc, yAcc)
    in traceback n m ("", "")

-- Function to print the alignment matrix with directions
printAlignmentMatrix :: String -> String -> IO ()
printAlignmentMatrix seq1 seq2 = do
    let (matrix, dirs) = needlemanWunsch seq1 seq2
        n = length seq1
        m = length seq2
    putStrLn "Alignment Matrix (Score, Direction):"
    mapM_ (\i -> 
        mapM_ (\j -> 
            putStr $ show (matrix!(i,j)) ++ "(" ++ show (dirs!(i,j)) ++ ")\t"
        ) [0..m]
        >> putStrLn ""
    ) [0..n]

-- Main function to demonstrate
main :: IO ()
main = do
    let seq1 = "AGCTGAC"
        seq2 = "AGCTTGC"
    putStrLn $ "Sequence 1: " ++ seq1
    putStrLn $ "Sequence 2: " ++ seq2
    let (dp, _) = needlemanWunsch seq1 seq2
    putStrLn $ "Optimal Alignment Score: " ++ 
        show (dp ! (length seq1, length seq2))
    
    let (aligned1, aligned2) = getAlignment seq1 seq2
    putStrLn "\nOptimal Alignment:"
    putStrLn aligned1
    putStrLn aligned2
    
    putStrLn "\nDetailed Alignment Matrix:"
    printAlignmentMatrix seq1 seq2