{-# Language OverloadedStrings #-}
{-# Language ViewPatterns #-}
{-# Language TupleSections #-}
{-# Language ScopedTypeVariables #-}

import Prelude hiding (Word)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Numeric.Natural
import Text.Read
import qualified Data.List.NonEmpty as NE
import Data.Function
import System.Random
import RandomNatural
import qualified Data.Char as C
import qualified System.IO as IO
import qualified System.FilePath as FP
import Tree

just  (Just a)  = a
right (Right a) = a

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





parseAnswer (C.toLower -> 'j') = Right Der
parseAnswer (C.toLower -> 'k') = Right Die
parseAnswer (C.toLower -> 'l') = Right Das
parseAnswer _ = Left "Did not understand answer"

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


-- Good ol' main :)
main = do
  IO.hSetBuffering IO.stdin  IO.NoBuffering
  IO.hSetBuffering IO.stdout IO.NoBuffering
  fileText <- TIO.readFile ("assets" FP.</> "data.txt") -- can error
  stdGen <- getStdGen
  let tree = makeTree fileText
  -- fmap (const ()) is to change the type from IO PlayState to IO () to match putStrLn
  either TIO.putStrLn (fmap (const ()) . play . (,stdGen)) tree


sw = Word {word = "Brief", gender = Der, value = 50, growth = Exp 25}
