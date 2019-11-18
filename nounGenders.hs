{-# LANGUAGE OverloadedStrings #-}
{-# Language ViewPatterns #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Numeric.Natural
import Text.Read

data Gender = Der | Die | Das deriving Show
data Growth = Lin | Exp Natural deriving Show

data Word = Word {word :: T.Text,
                  gender :: Gender,
                  value :: Natural,
                  growth :: Growth} 
                  deriving Show

data Tree a = 
  Branch (Tree a) (Tree a) a -- invariant: the a is the sum of the a's in the subtrees
  | Leaf a

-- Converts T.Text to String and performs readEither
-- dirty, but I couldn't find any way to do this in the docs.
tReadEither :: Read a => T.Text -> T.Text -> Either T.Text a
tReadEither leftVal showable = 
  case readMaybe $ show showable of
    Just val -> Right val
    Nothing  -> Left leftVal

parseGrowth :: T.Text -> T.Text -> Either T.Text Growth
parseGrowth errMsg str = (return . Exp) =<< tReadEither errMsg str

--removeEmpties :: [T.Text] -> [T.Text]
removeEmpties = foldr (\elm acc -> if (null elm) then acc else elm:acc) []

removeComments :: [T.Text] -> [T.Text]
removeComments = foldr (\elm acc -> if (T.take 2 elm == "//") then [] else elm:acc) []

parseGender :: T.Text -> Either T.Text Gender
parseGender "Der" = Right Der
parseGender "Die" = Right Die
parseGender "Das" = Right Das
parseGender otherGender = Left ("Parse error in file: invalid gender" `T.append` otherGender)

-- Little ugly function to ease >>= in parseWord
createWord word value growth gender = 
  Word {word=word, gender=gender, value=value, growth=growth}

parseWord [genderStr, word] = (createWord word 0 Lin) <$> parseGender genderStr
parseWord [genderStr, word, valueStr] = do
  gender <- parseGender genderStr 
  value  <- tReadEither ("Parse error in file: invalid number: " `T.append` valueStr) valueStr
  return $ createWord word value Lin gender
parseWord [genderStr, word, valueStr, growthStr] = do
  gender <- parseGender genderStr 
  value  <- tReadEither ("Parse error in file: invalid value number: "  `T.append` valueStr) valueStr
  growth <- parseGrowth ("Parse error in file: invalid growth number: " `T.append` growthStr) growthStr
  return $ createWord word value Lin gender
parseWord _ = Left "Parse error in file: Haven't bothered to write good enough error messages for me to tell you what"



main = do
  fileText <- TIO.readFile "data.txt"
  let input = sequence $ fmap parseWord $ removeEmpties  $ removeComments <$> T.words <$> (T.lines fileText)
--((fmap T.words) . T.lines) <$> TIO.readFile "data.txt"
  putStrLn (show input)
  --sequence2 (fmap2 TIO.putStr input)
--((`T.append` "\n") <$> input))
