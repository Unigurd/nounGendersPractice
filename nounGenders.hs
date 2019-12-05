{-# LANGUAGE OverloadedStrings #-}
{-# Language ViewPatterns #-}
import Prelude hiding (Word)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Numeric.Natural
import Text.Read
import Data.List.NonEmpty as NE
import Data.Function
import System.Random
import RandomNatural

data Growth = Lin | Exp Natural deriving Show
data Gender = Der | Die | Das deriving (Show, Eq)

data Word = Word {word :: T.Text,
                  gender :: Gender,
                  value :: Natural,
                  growth :: Growth} 
                  deriving Show

data Tree a = 
  Branch (Tree a) Natural (Tree a)
  | Leaf a Natural
  deriving Show

treeVal (Leaf _ val) = val
treeVal (Branch _ val _) = val


-- Converts T.Text to String and performs readEither
-- dirty, but I couldn't find any way to do this in the docs.
tReadEither :: Read a => T.Text -> T.Text -> Either T.Text a
tReadEither leftVal text =  -- Omskriv til viewpatterns
  case readMaybe $ T.unpack text of
    Just val -> Right val
    Nothing  -> Left leftVal

parseGrowth :: T.Text -> T.Text -> Either T.Text Growth
parseGrowth errMsg str = (return . Exp) =<< tReadEither errMsg str

--removeEmpties :: [T.Text] -> [T.Text]
removeEmpties = foldr (\elm acc -> case (nonEmpty elm) of 
                                     Just nonEmptyElm -> nonEmptyElm:acc 
                                     Nothing          -> acc) 
                      []

removeComments :: [T.Text] -> [T.Text]
removeComments = foldr (\elm acc -> if (T.take 2 elm == "//") then [] else elm:acc) []

parseGender :: T.Text -> Either T.Text Gender
parseGender "Der" = Right Der
parseGender "Die" = Right Die
parseGender "Das" = Right Das
parseGender otherGender = Left ("Parse error in file: invalid gender: " `T.append` otherGender)

nonEmptyEither _      (nonEmpty -> Just nonE) = Right nonE
nonEmptyEither errMsg (nonEmpty -> Nothing)   = Left errMsg

-- Little ugly function to ease >>= in parseWord
createWord word value growth gender = 
  Word {word=word, gender=gender, value=value, growth=growth}

parseWord (genderStr :| [word]) = (createWord word 0 Lin) <$> parseGender genderStr
parseWord (genderStr :| [word, valueStr]) = do
  gender <- parseGender genderStr 
  value  <- tReadEither ("Parse error in file: invalid value number: " `T.append` valueStr) valueStr
  return $ createWord word value Lin gender
parseWord (genderStr :| [word, valueStr, growthStr]) = do
  gender <- parseGender genderStr 
  value  <- tReadEither ("Parse error in file: invalid value number: "  `T.append` valueStr) valueStr
  growth <- parseGrowth ("Parse error in file: invalid growth number: " `T.append` growthStr) growthStr
  return $ createWord word value growth gender
parseWord _ = Left "Parse error in file: Haven't bothered to write good enough error messages for me to tell you what"

--largestValue = foldl (\acc elm -> if value elm > acc then value elm else acc)
parseInput input = 
  traverse parseWord 
             =<< (nonEmptyEither "Data file was empty"  
              $ removeEmpties 
              $ removeComments 
             <$> T.words 
             <$> (T.lines input))

-- maxVal list >= value x for all x in list
leafify list = fmap (\x -> (Leaf x (maxVal list - value x + 1 ))) list

-- I don't like that maximum doesn't check non-emptyness statically
maxVal = maximum . values

pairUp :: NonEmpty (Tree Word) -> NonEmpty (Tree Word)
pairUp (a:|[b]) = (Branch a ((treeVal a) + (treeVal b)) b):|[]
pairUp (a:|(b:restHead:restTail)) = 
  (Branch a ((treeVal a) + (treeVal b)) b) `cons` (pairUp (restHead:|restTail))
pairUp (a:|[])        = a:|[]

rotate :: NonEmpty a -> NonEmpty a
rotate list = (NE.last list):|(NE.init list)

-- rotate makes sure that no single leaf  can become the right child of the root
-- (Except when the 3 <= the number of elements <= 6
-- Otherwise the following could happen:
-- For example, (a b c d e) ->> (ab cd e) ->> (abcd e)
buildTree :: NonEmpty (Tree Word) -> Tree Word
buildTree (result:|[]) = result
buildTree list         = buildTree $ rotate $ pairUp list

values :: Functor f => f Word -> f Natural
values = fmap value

pickWord :: Tree a -> Natural -> a
pickWord (Leaf word _) _ = word
pickWord (Branch left _ right) searchVal =
  if searchVal <= treeVal left
  then pickWord left searchVal
  else pickWord right searchVal

parseAnswer :: T.Text -> Either T.Text Gender
parseAnswer (T.toLower -> "der") = Right Der
parseAnswer (T.toLower -> "die") = Right Die
parseAnswer (T.toLower -> "das") = Right Das
parseAnswer answer               = Left $ T.append "Could not understand " answer

tShow = show . T.pack

play :: RandomGen g => Tree Word -> g -> IO ()
play tree randGen = do
  let (index,_) = randomR (1, treeVal tree) randGen
  let quizWord = pickWord tree $ index
  let wordStr = word quizWord
  TIO.putStrLn wordStr
  rawAnswer <- TIO.getLine
  let answer = T.toLower rawAnswer
  TIO.putStrLn 
    (case (== gender quizWord) <$> parseAnswer answer of
       Left errMsg -> errMsg 
       Right True  -> "Correct"
       Right False -> "Wrong! >:(")

main = do
  fileText <- TIO.readFile "data.txt"
  stdGen <- getStdGen
  let tree = do
      input <- parseInput fileText
      let leaves = leafify input 
      let tree = buildTree leaves
      return tree
  either TIO.putStrLn (`play` stdGen) tree
