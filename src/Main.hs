{-# Language OverloadedStrings #-}
{-# Language ViewPatterns #-}
{-# Language TupleSections #-}

import Prelude hiding (Word)
import qualified Data.Text.IO    as TIO
import qualified System.Random   as R
import qualified Data.Char       as C
import qualified System.IO       as IO
import qualified System.FilePath as FP
import qualified Tree            as TR

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

parseAnswer (C.toLower -> 'j') = Right TR.Der
parseAnswer (C.toLower -> 'k') = Right TR.Die
parseAnswer (C.toLower -> 'l') = Right TR.Das
parseAnswer _ = Left "Did not understand answer"

getAnswers :: TR.Gender -> IO (Maybe Bool)
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

playRound :: TR.RandomTree TR.Word -> IO (Maybe ((), TR.RandomTree TR.Word))
playRound (tree, randGen) = do
  let (index,newGen) = R.randomR (1, TR.treeVal tree) randGen
  let wordToGuess = TR.pickWord tree index
  TIO.putStrLn $ TR.word wordToGuess
  playState <- getAnswers $ TR.gender wordToGuess
  TIO.putStrLn ""
  return $ do 
    isCorrectGuess <- playState
    let newTree = TR.updateTree isCorrectGuess tree index
    return ((), (newTree, newGen))

--  let a = do 
--            isCorrectGuess <- playState
--            let newTree = updateTree isCorrectGuess tree index
--            return ((), (newTree, newGen))
--  TIO.putStrLn $ format $ fst $ snd $ just a
--  return a

play :: TR.RandomTree TR.Word -> IO [()]
play = unfoldrM playRound 


-- Good ol' main :)
main = do
  IO.hSetBuffering IO.stdin  IO.NoBuffering
  IO.hSetBuffering IO.stdout IO.NoBuffering
  fileText <- TIO.readFile ("assets" FP.</> "data.txt") -- can error
  stdGen <- R.getStdGen
  let tree = TR.makeTree fileText
  -- fmap (const ()) is to change the type from IO PlayState to IO () to match putStrLn
  either TIO.putStrLn (fmap (const ()) . play . (,stdGen)) tree


--sw = Word {word = "Brief", gender = Der, value = 50, growth = Exp 25}
