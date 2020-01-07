{-# Language OverloadedStrings #-}
{-# Language ViewPatterns #-}
{-# Language TupleSections #-}
{-# Language ScopedTypeVariables #-}

import Prelude hiding (Word)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Numeric.Natural
import Text.Read
import Data.List.NonEmpty as NE
import Data.Function
import System.Random
import RandomNatural
import Data.Char as C
import System.IO as IO

-- Specifies whether the likelihood of a word decreases linearly or exponentially when guessed correctly
-- Inspired by TCP congestion control, whenever an error is made, the likelihood of that word
-- skyrockets and moves exponentially back to half of what it what before the error,
-- where it goes back to growing linearly.
-- The Natural is the limit for when the exponential growth goes back to linear.
data Growth = Lin | Exp Natural deriving Show

data Gender = Der | Die | Das deriving (Show, Eq)

-- A word consist of the word itself, its gender, the manner in which the likelihood of the word grows,
-- and a value for which holds that the likelihood of the word being chosen increases as the value increases.
data Word = Word {word :: T.Text,
                  gender :: Gender,
                  growth :: Growth,
                  value :: Natural}
                  deriving Show


-- Each leaf has an element of type a and a Natural n, where the likelihood of the a being chosen
-- is n/(sum of n's for all leaves)
-- For Words, n is max of all values of words - the value of this word
-- Each branch has a Natural n which should be the sum of n's in each of the subtrees.
data Tree a = 
  Branch (Tree a) Natural (Tree a)
  | Leaf a Natural
  deriving Show

type RandomTree a = (Tree a, StdGen)


-- Returns the topmost Natural of a tree.
treeVal (Leaf _ val) = val
treeVal (Branch _ val _) = val

-- Converts T.Text to String and performs readEither
tReadEither :: Read a => T.Text -> T.Text -> Either T.Text a
tReadEither leftVal text =  -- Omskriv til viewpatterns
  case readMaybe $ T.unpack text of
    Just val -> Right val
    Nothing  -> Left leftVal

-- If there is a growth value present in the data, the growth must be exponential.
-- Therefore this only parses Exp's
parseGrowth :: T.Text -> T.Text -> Either T.Text Growth
parseGrowth errMsg str = (return . Exp) =<< tReadEither errMsg str

-- Removes empty inner lists from a list of lists,
-- returning a list of NonEmpty's
removeEmpties = foldr (\elm acc -> case (NE.nonEmpty elm) of 
                                     Just nonEmptyElm -> nonEmptyElm:acc 
                                     Nothing          -> acc) 
                      []

-- Uppercases the first character of a string if non-empty
upperFirst (T.uncons -> Just (first, rest)) = (C.toUpper first) `T.cons` rest
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

nonEmptyEither _      (NE.nonEmpty -> Just nonE) = Right nonE
nonEmptyEither errMsg (NE.nonEmpty -> Nothing)   = Left errMsg

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
pairUp (a:|[b]) = (Branch a ((treeVal a) + (treeVal b)) b):|[]
pairUp (a:|(b:restHead:restTail)) = 
  (Branch a ((treeVal a) + (treeVal b)) b) `cons` (pairUp (restHead:|restTail))
pairUp (a:|[])        = a:|[]

-- Rotates a list so the last element becomes the first.
rotate :: NonEmpty a -> NonEmpty a
rotate list = (NE.last list):|(NE.init list)

-- Builds a single tree out of a NonEmpty of trees.
-- Rotate makes sure that no single leaf  can become the right child of the root
-- (Except when the 3 <= the number of elements <= 6
-- Otherwise the following could happen:
-- For example, (a b c d e) ->> (ab cd e) ->> (abcd e)
-- Has a bad name that will likely be changed
buildTree :: NonEmpty (Tree Word) -> Tree Word
buildTree (result:|[]) = result
buildTree list         = buildTree $ rotate $ pairUp list

-- maps a functor of words to a functor of the values of those words
values :: Functor f => f Word -> f Natural
values = fmap value

-- Thinking of a leaf's Natural as its range of indexes,
-- this function returns the word at the supplied index
pickWord :: Tree a -> Natural -> a
pickWord (Leaf word _) _ = word
pickWord (Branch left _ right) searchVal =
  if searchVal <= treeVal left
  then pickWord left (searchVal)
  else pickWord right (searchVal - treeVal left)


-- Shows a Text instead of a String
tShow :: Show a => a -> T.Text
tShow = T.pack . show

-- Returns a random word in a tree
randomWord (tree, randGen) = (quizWord, (tree, newGen))
  where
    (index,newGen) = randomR (1, treeVal tree) randGen
    quizWord = pickWord tree $ index

randomWords :: RandomTree Word -> NonEmpty Word
randomWords randomTree = myIterate randomWord randomTree

data Success = 
    Success 
  | Failure

divf a b = floor (af / b)
  where
    af = fromIntegral a

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

parseAnswer (toLower -> 'j') = Right Der
parseAnswer (toLower -> 'k') = Right Die
parseAnswer (toLower -> 'l') = Right Das
parseAnswer _ = Left "Did not understand answer"

shouldContinue 'q' = False
shouldContinue  n  = True

getAnswers :: Gender -> IO (Maybe Bool)
getAnswers realGender = exitTest realGender
  where 
    exitTest realGender = do
      rawAnswer <- getChar
      case rawAnswer of
        'q' -> return Nothing
        _  -> correctnessTest realGender $ parseAnswer rawAnswer

    correctnessTest realGender (Right answer) =
      if answer == realGender
      then return $ return True
      else do
        maybeAnswer <- exitTest realGender
        return $ do
          answer <- maybeAnswer
          return $ False && answer -- introduces non-tail recursiveness
    correctnessTest realGender (Left _) =
      exitTest realGender

playRound :: RandomTree Word -> IO (Maybe ((), RandomTree Word))
playRound (tree, randGen) = do
  let (index,newGen) = randomR (1, treeVal tree) randGen
  let wordToGuess = pickWord tree index
  TIO.putStrLn $ word wordToGuess
  (playState :: Maybe Bool) <- getAnswers $ gender wordToGuess
  TIO.putStrLn ""
  return $ do 
    (isCorrectGuess :: Bool) <- playState
    let newTree = updateTree isCorrectGuess tree index
    return ((), (newTree, newGen))

--  let a = do 
--            isCorrectGuess <- playState
--            let newTree = updateTree isCorrectGuess tree index
--            return ((), (newTree, newGen))
--  TIO.putStrLn $ format $ fst $ snd $ just a
--  return a

play :: RandomTree Word -> IO [()]
play = unfoldrM playRound 

myIterate :: (b -> (a,b)) -> b -> NonEmpty a
myIterate f x = fmap fst $ NE.iterate (\(a,b) -> f b) $ f x

-- Makes an entire tree from raw input
makeTree input = buildTree <$> leafify <$> parseInput input

mySequence :: Monad m =>  (a -> Bool) -> [m a] -> m [a]
mySequence _ [] = return []
mySequence pred (a:as) = do
  a2 <- a
  if pred a2
  then return []
  else do
    as2 <- mySequence pred as
    return (a2:as2)

mySequenceNE pred (a:|as) = do
  a2 <- a
  if pred a2
  then return (a2:|[])
  else do
    as2 <- mySequence pred as
    return (a2:|as2)

-- Stolen from
-- https://hackage.haskell.org/package/monad-loops-0.4.3/docs/src/Control-Monad-Loops.html#unfoldrM
unfoldrM :: (Monad m) => (a -> m (Maybe (b,a))) -> a -> m [b]
unfoldrM f = go
    where go z = do
            x <- f z
            case x of
                Nothing         -> return []
                Just (x, z')    -> do
                        xs <- go z'
                        return (x:xs)


-- Good ol' main :)
main = do
  IO.hSetBuffering stdin NoBuffering
  IO.hSetBuffering stdout NoBuffering
  fileText <- TIO.readFile "data.txt"
  stdGen <- getStdGen
  let tree = makeTree fileText
  -- fmap (const ()) is to change the type from IO PlayState to IO () to match putStrLn
  either TIO.putStrLn (fmap (const ()) . play . (,stdGen)) tree

just  (Just a)  = a
right (Right a) = a

sw = Word {word = "Brief", gender = Der, value = 50, growth = Exp 25}
