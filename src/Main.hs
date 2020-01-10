{-# Language OverloadedStrings #-}
{-# Language ViewPatterns #-}
{-# Language TupleSections #-}

import Prelude hiding (Word)
import qualified Data.Text.IO    as TIO
import System.IO
  (hSetBuffering, stdin, stdout, BufferMode(NoBuffering))
import System.Random (randomR, getStdGen)
import System.FilePath ((</>))
import Play (play)
import Tree (makeTree)

just  (Just a)  = a
right (Right a) = a



-- Good ol' main :)
main = do
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  fileText <- TIO.readFile ("assets" </> "data.txt") -- can error
  stdGen <- getStdGen
  let tree = makeTree fileText
  -- fmap (const ()) is to change the type from IO PlayState to IO () to match putStrLn
  either TIO.putStrLn (fmap (const ()) . play . (,stdGen)) tree


--sw = Word {word = "Brief", gender = Der, value = 50, growth = Exp 25}
