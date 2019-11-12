{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Gender = Der | Die | Das

data Tree a = 
  Branch (Tree a) (Tree a) a -- invariant: the a is the sum of the a's in the subtrees
  | Leaf a


main = do
  input <- T.lines <$> TIO.readFile "data.txt"
  sequence (TIO.putStr <$> ((`T.append` "\n") <$> input))
  
