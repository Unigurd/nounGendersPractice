{-# Language ViewPatterns #-}
{-# Language OverloadedStrings #-}

module Play where

import qualified Data.Text.IO as TIO
import System.Random (randomR)
import Prelude hiding (Word)
import Data.Char (toLower)
import Misc (unfoldrM)
import Tree
  (RandomTree, treeVal, updateTree,
  Word, word, pickWord,
  Gender(Der, Die, Das), gender)

parseAnswer (toLower -> 'j') = Right Der
parseAnswer (toLower -> 'k') = Right Die
parseAnswer (toLower -> 'l') = Right Das
parseAnswer _ = Left "Did not understand answer"



-- Gets answers until correct or 'q'
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
  playState <- getAnswers $ gender wordToGuess
  TIO.putStrLn ""
  return $ do
    isCorrectGuess <- playState
    let newTree = updateTree isCorrectGuess tree index
    return ((), (newTree, newGen))

play :: RandomTree Word -> IO [()]
play = unfoldrM playRound
