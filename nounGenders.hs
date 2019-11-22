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

-- Copied from 
-- https://hackage.haskell.org/package/random-1.1/docs/src/System.Random.html#randomIvalIntegral
-- The two integer functions below take an [inclusive,inclusive] range.
randomIvalIntegral :: (RandomGen g, Integral a) => (a, a) -> g -> (a, g)
randomIvalIntegral (l,h) = randomIvalInteger (toInteger l, toInteger h)

randomIvalInteger :: (RandomGen g, Num a) => (Integer, Integer) -> g -> (a, g)
randomIvalInteger (l,h) rng
 | l > h     = randomIvalInteger (h,l) rng
 | otherwise = case (f 1 0 rng) of (v, rng') -> (fromInteger (l + v `mod` k), rng')
     where
       (genlo, genhi) = genRange rng
       b = fromIntegral genhi - fromIntegral genlo + 1

       -- Probabilities of the most likely and least likely result
       -- will differ at most by a factor of (1 +- 1/q).  Assuming the RandomGen
       -- is uniform, of course

       -- On average, log q / log b more random values will be generated
       -- than the minimum
       q = 1000
       k = h - l + 1
       magtgt = k * q

       -- generate random values until we exceed the target magnitude 
       f mag v g | mag >= magtgt = (v, g)
                 | otherwise = v' `seq`f (mag*b) v' g' where
                        (x,g') = next g
                        v' = (v * b + (fromIntegral x - fromIntegral genlo))

integerToNatural :: Integer -> Natural
integerToNatural i
  | i < 0 = fromInteger (abs i) - 1
  | True  = fromInteger i
integralToNatural = integerToNatural . toInteger

instance Random Natural where
  randomR (a,b) g = (nat,g') where
    (int,g') = randomIvalIntegral (a::Natural, b) g
    nat = integralToNatural int
    
  random = randomR (0, (integralToNatural (maxBound::Int)))

data Gender = Der | Die | Das deriving Show
data Growth = Lin | Exp Natural deriving Show

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

main = do
  fileText <- TIO.readFile "data.txt"
  stdGen <- getStdGen
  let comp = do
      input <- parseInput fileText
      let leaves = leafify input 
      let tree = buildTree leaves
      let (index,_) = randomR (1, treeVal tree) stdGen
      let word = pickWord tree $ index
      return index
  putStrLn (show comp)
