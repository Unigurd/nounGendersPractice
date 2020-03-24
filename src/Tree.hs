{-# Language ViewPatterns #-}
{-# Language OverloadedStrings #-}

module Tree where

import Prelude hiding (Word)
import qualified Data.Text          as T
import qualified Numeric.Natural    as N
import qualified Text.Read          as TR
import qualified RandomNatural      as RN
import Data.List.NonEmpty           as NE
  (NonEmpty((:|)), nonEmpty, cons, init, last)
import System.Random (StdGen, randomR, getStdGen)
import Data.Char (toUpper)
import Misc (divf, tShow)

-- Specifies whether the likelihood of a word decreases linearly or exponentially when guessed correctly
-- Inspired by TCP congestion control, whenever an error is made, the likelihood of that word
-- skyrockets and moves exponentially back to half of what it what before the error,
-- where it goes back to growing linearly.
-- The Natural is the limit for when the exponential growth goes back to linear.
data Growth = Lin | Exp N.Natural deriving Show

data Gender = Der | Die | Das deriving (Show, Eq)

-- A word consist of the word itself, its gender, the manner in which the likelihood of the word grows,
-- and a value for which holds that the likelihood of the word being chosen increases as the value increases.
data Word = Word {word :: T.Text,
                  gender :: Gender,
                  growth :: Growth,
                  value :: N.Natural}
                  deriving Show


-- Each leaf has an element of type a and a Natural n, where the likelihood of the a being chosen
-- is n/(sum of n's for all leaves)
-- For Words, n is max of all values of words - the value of this word
-- Each branch has a Natural n which should be the sum of n's in each of the subtrees.
data Tree a =
  Branch (Tree a) N.Natural (Tree a)
  | Leaf a N.Natural
  deriving Show

type RandomTree a = (Tree a, StdGen)


-- Returns the topmost Natural of a tree.
treeVal (Leaf _ val) = val
treeVal (Branch _ val _) = val

-- Converts T.Text to String and performs readEither
tReadEither :: Read a => T.Text -> T.Text -> Either T.Text a
tReadEither leftVal text =  -- Omskriv til viewpatterns
  case TR.readMaybe $ T.unpack text of
    Just val -> Right val
    Nothing  -> Left leftVal

-- If there is a growth value present in the data, the growth must be exponential.
-- Therefore this only parses Exp's
parseGrowth :: T.Text -> T.Text -> Either T.Text Growth
parseGrowth errMsg str = (return . Exp) =<< tReadEither errMsg str

-- Removes empty inner lists from a list of lists,
-- returning a list of NonEmpty's
removeEmpties = foldr (\elm acc -> case (nonEmpty elm) of
                                     Just nonEmptyElm -> nonEmptyElm:acc
                                     Nothing          -> acc)
                      []

-- Uppercases the first character of a string if non-empty
upperFirst (T.uncons -> Just (first, rest)) = (toUpper first) `T.cons` rest
upperFirst emptyStr                         = emptyStr

-- Returns the string with the first char uppercase and the rest lower.
capitalizeWord = upperFirst . T.toLower

-- Removes all strings after a string beginning with // in a list of strings
removeComments :: [T.Text] -> [T.Text]
removeComments = foldr (\elm acc -> if (T.take 2 elm == "//") then [] else elm:acc) []

-- parses the three genders regardless of case
parseGender :: T.Text -> T.Text -> Either T.Text Gender
parseGender _ (T.toLower -> "der") = Right Der
parseGender _ (T.toLower -> "die") = Right Die
parseGender _ (T.toLower -> "das") = Right Das
parseGender errMsg otherGender = Left (errMsg `T.append` otherGender)

nonEmptyEither _      (nonEmpty -> Just nonE) = Right nonE
nonEmptyEither errMsg (nonEmpty -> Nothing)   = Left errMsg

-- Little ugly function to ease >>= in parseWord
createWord word value growth gender =
  Word {word=word, gender=gender, value=value, growth=growth}

-- parses everything needed for a Word
-- Handles different amounts of information given.
--
-- Only gender and word given
parseWord (genderStr :| [word]) = (createWord (capitalizeWord word) 0 Lin) <$> parseGender "Parse error in file: invalid gender: " genderStr
-- Gender, word and value given
parseWord (genderStr :| [word, valueStr]) = do
  gender <- parseGender "Parse error in file: invalid gender: " genderStr
  value  <- tReadEither ("Parse error in file: invalid value number: " `T.append` valueStr) valueStr
  return $ createWord (capitalizeWord word) value Lin gender
-- Everything given
parseWord (genderStr :| [word, valueStr, growthStr]) = do
  gender <- parseGender "Parse error in file: invalid gender: " genderStr
  value  <- tReadEither ("Parse error in file: invalid value number: "  `T.append` valueStr) valueStr
  growth <- parseGrowth ("Parse error in file: invalid growth number: " `T.append` growthStr) growthStr
  return $ createWord (capitalizeWord word) value growth gender
-- None of the above cases
parseWord _ = Left "Parse error in file: Haven't bothered to write good enough error messages for me to tell you what"

-- Parses raw Text input into a NonEmpty of Words (if possible),
-- so it doesn't impose the tree structure
parseInput :: T.Text -> Either T.Text (NonEmpty Word)
parseInput input =
  traverse parseWord
             =<< (nonEmptyEither "Data file was empty"
              $ removeEmpties
              $ removeComments
             <$> T.words
             <$> (T.lines input))


-- Puts words into leaves, where the Natural in the leaves is the max value for any word - that particular word's value
-- This negation is to make 0 an upper bound on likelihood instead of a lower
-- leafify list = fmap (\x -> (Leaf x (maxVal list - value x + 1 ))) list
leafify list = fmap (\x -> (Leaf x (value x + 1 ))) list

-- I don't like that maximum raises an error on nonemptyness
-- so myMaximum can only work on NonEmpty
myMaximum :: Ord a => NonEmpty a -> a
myMaximum = maximum

maxVal = myMaximum . values

-- Returns a NonEmpty of branches where the subtrees are pairs of trees in the originial NonEmpty.
-- In the case of an odd length of the NonEmpty the last tree is included unchanged.
pairUp :: NonEmpty (Tree Word) -> NonEmpty (Tree Word)
pairUp (a :| [b]) = (Branch a ((treeVal a) + (treeVal b)) b) :| []
pairUp (a :| (b:restHead:restTail)) =
  (Branch a ((treeVal a) + (treeVal b)) b) `cons` (pairUp (restHead :| restTail))
pairUp (a :| [])        = a :| []

-- Rotates a list so the last element becomes the first.
rotate :: NonEmpty a -> NonEmpty a
rotate list = (NE.last list) :| (NE.init list)

-- Builds a single tree out of a NonEmpty of trees.
-- Rotate makes sure that no single leaf  can become the right child of the root
-- (Except when the 3 <= the number of elements <= 6
-- Otherwise the following could happen:
-- For example, (a b c d e) ->> (ab cd e) ->> (abcd e)
-- Has a bad name that will likely be changed
buildTree :: NonEmpty (Tree Word) -> Tree Word
buildTree (result :| []) = result
buildTree list         = buildTree $ rotate $ pairUp list

-- maps a functor of words to a functor of the values of those words
values :: Functor f => f Word -> f N.Natural
values = fmap value

-- Thinking of a leaf's Natural as its range of indexes,
-- this function returns the word at the supplied index
pickWord :: Tree a -> N.Natural -> a
pickWord (Leaf word _) _ = word
pickWord (Branch left _ right) searchVal =
  if searchVal <= treeVal left
  then pickWord left (searchVal)
  else pickWord right (searchVal - treeVal left)


-- Returns a random word in a tree
randomWord (tree, randGen) = (quizWord, (tree, newGen))
  where
    (index,newGen) = randomR (1, treeVal tree) randGen
    quizWord = pickWord tree $ index

-- I think I should look into lenses
updateWord True wordToUpdate =
  case growth wordToUpdate of
    Lin ->
      Word {word   = word wordToUpdate,
            gender = gender wordToUpdate,
            value  = if value wordToUpdate == 0 then 0 else value wordToUpdate - 1,
            growth = Lin}
    Exp n ->
      if newVal <= n
      then Word
           {word   = word wordToUpdate,
            gender = gender wordToUpdate,
            value  = n,
            growth = Lin}
      else Word
           {word   = word wordToUpdate,
            gender = gender wordToUpdate,
            value  = newVal,
            growth = Exp n}
        where newVal = value wordToUpdate `divf` 1.5

updateWord False wordToUpdate =
  case growth wordToUpdate of
    Lin ->
      Word {word   = word wordToUpdate,
            gender = gender wordToUpdate,
            value  = newVal,
            growth = Exp newN}
      where
        -- Adding 1 for the case where the value is zero
        newN   = (value wordToUpdate + 1) * 2
        newVal = (value wordToUpdate + 1)* 6
    Exp n ->
      Word {word   = word wordToUpdate,
            gender = gender wordToUpdate,
            value  = newVal,
            growth = Exp n}
      where
        -- Adding 1 for the case where the value is zero
        newVal = (value wordToUpdate + 1)* 6

-- n+1 because we want each word to have an index range of at least 1 so it can be found
updateTree success (Leaf word _) _ = Leaf updatedWord ((value updatedWord) + 1)
  where updatedWord = updateWord success word
updateTree success (Branch left n right) searchVal =
  if searchVal <= treeVal left
  then Branch newLeft newLN right
  else Branch left newRN newRight
    where
      newLeft  = updateTree success left (searchVal)
      newRight = updateTree success right (searchVal - treeVal left)
      newLN    = treeVal newLeft + treeVal right
      newRN    = treeVal left + treeVal newRight

-- Returns a human-readable version of a Tree
format :: Tree Word -> T.Text
format = T.unlines . help ""
  where
    help spaces (Branch l n r) =
      spaces `T.append` (tShow n)
             : ((help newSpaces l) ++ (help newSpaces r))
        where newSpaces = T.cons ' ' spaces
    help spaces (Leaf w n) =
      [spaces `T.append` T.unwords [tShow n, (tShow $ gender w), word w]]

makeTree input = buildTree <$> leafify <$> parseInput input
