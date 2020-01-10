{-# Language TupleSections #-}

import qualified Data.Text.IO    as TIO
import System.IO
  (hSetBuffering, stdin, stdout, BufferMode(NoBuffering))
import System.Random (getStdGen)
import System.FilePath ((</>))
import Play (play)
import Tree (makeTree)

-- Good ol' main :)
main = do
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  fileText <- TIO.readFile ("assets" </> "data.txt") -- can error
  stdGen <- getStdGen
  let tree = makeTree fileText
  -- fmap (const ()) is to change the type from IO PlayState to IO () to match putStrLn
  either TIO.putStrLn (fmap (const ()) . play . (,stdGen)) tree
