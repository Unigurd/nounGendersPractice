{-# Language TupleSections #-}
{-# Language ScopedTypeVariables #-}

import Data.Text (Text)
import qualified Data.Text.IO    as TIO
import System.IO
  (hSetBuffering, stdin, stdout, BufferMode(NoBuffering))
import System.Random (getStdGen)
import System.FilePath ((</>))
import Play (play)
import Tree (makeTree)
import Control.Exception (try)

file = "data.txt"

-- Good ol' main :)
main = do
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  (eitherFileText :: Either IOError Text) <- try $ TIO.readFile ("assets" </> file)
  case eitherFileText of
    Right fileText -> do
      let tree = makeTree fileText
      stdGen <- getStdGen
      either TIO.putStrLn (fmap (const ()) . play . (,stdGen)) tree
    Left _ -> putStrLn ("Could not open file " ++ file)
