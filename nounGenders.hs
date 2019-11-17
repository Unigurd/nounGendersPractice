{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Numeric.Natural
import Text.Read

data Gender = Der | Die | Das
data Growth = Lin | Exp Natural

-- We want an Exp when there is a fourth word, a number, in the file.
-- Otherwise we want a Lin. So we only care about creating Exp's when we read
-- yuck!
instance Read Growth where
  readPrec = Exp . read

data Word = Word {word :: T.Text,
                  gender :: Gender,
                  value :: Natural,
                  growth :: Growth}

data Tree a = 
  Branch (Tree a) (Tree a) a -- invariant: the a is the sum of the a's in the subtrees
  | Leaf a

-- Converts T.Text to String and performs readEither
-- dirty, but I couldn't find any way to do this in the docs.
tReadEither :: Read a => T.Text -> T.Text -> Either T.Text a
tReadEither leftVal rightVal = 
  case readMaybe $ show leftVal of
    Just val -> Right val
    Nothing  -> Left leftVal

removeEmpties = foldr (\elm acc -> if (elm == "") then acc else elm:acc) []

parseGender :: T.Text -> Either T.Text Gender
parseGender "Der" = Right Der
parseGender "Die" = Right Die
parseGender "Das" = Right Das
parseGender otherGen = Left ("Parse error in file: invalid gender" `T.append` otherGen)

-- Little ugly function to ease >>= in parseWord
createWord word value growth gender = 
  Word {word=word, gender=gender, value=value, growth=growth}

parseWord [genderStr, word] = (createWord word 0 Lin) <$> parseGender genderStr
parseWord [genderStr, word, valueStr] = do
  gender <- parseGender genderStr 
  value  <- tReadEither ("Parse error in file: invalid number" `T.append` valueStr) valueStr
  return $ createWord word value Lin gender
parseWord [genderStr, word, valueStr, growthStr] = do
  gender <- parseGender genderStr 
  value  <- tReadEither ("Parse error in file: invalid value number: "  `T.append` valueStr) valueStr
  growth <- tReadEither ("Parse error in file: invalid growth number: " `T.append` growthStr) growthStr
  return $ createWord word value Lin gender

main = do
  fileText <- TIO.readFile "data.txt"
  let input = T.words <$> (removeEmpties $ T.lines fileText)
--((fmap T.words) . T.lines) <$> TIO.readFile "data.txt"
  putStr (show input)
  --sequence2 (fmap2 TIO.putStr input)
--((`T.append` "\n") <$> input))
  
