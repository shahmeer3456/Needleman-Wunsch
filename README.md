# Needleman-Wunsch Algorithm Implementation in Haskell

This repository contains an implementation of the Needleman-Wunsch algorithm for sequence alignment in Haskell. The algorithm is used for global alignment of two sequences, particularly useful in bioinformatics for aligning protein or nucleotide sequences.

## Features

- Implementation of the Needleman-Wunsch dynamic programming algorithm
- Scoring system with match, mismatch, and gap penalties
- Traceback functionality to show the actual sequence alignment
- Detailed alignment matrix visualization
- Support for any character-based sequence alignment

## Usage

To run the program:

1. Make sure you have GHC (Glasgow Haskell Compiler) installed
2. Compile the program:
   ```
   ghc algo1.hs
   ```
3. Run the executable:
   ```
   ./algo1
   ```

The program will demonstrate alignment with example sequences "AGCTGAC" and "AGCTTGC".

## Implementation Details

- Scoring scheme:
  - Match: +1
  - Mismatch: -1
  - Gap: -1
- Uses Data.Array for efficient matrix operations
- Includes direction tracking for optimal alignment reconstruction 