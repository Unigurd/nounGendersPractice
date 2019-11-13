{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Gender = Der | Die | Das

data Tree a = 
  Branch (Tree a) (Tree a) a -- invariant: the a is the sum of the a's in the subtrees
  | Leaf a


fmap2 f a = (fmap f) <$> a
--sequence2 = sequence . fmap sequence

removeEmpties = 

parseWord ((gender:word):rest) = gender gen

main = do
  input <- fmap2 T.words $ fmap T.lines (TIO.readFile "data.txt")
--((fmap T.words) . T.lines) <$> TIO.readFile "data.txt"
  putStr (show input)
  --sequence2 (fmap2 TIO.putStr input)
--((`T.append` "\n") <$> input))
  
