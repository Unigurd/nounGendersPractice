module Misc where

import qualified Data.Text as T

divf a b = floor (af / b)
  where
    af = fromIntegral a

-- Shows a Text instead of a String
tShow :: Show a => a -> T.Text
tShow = T.pack . show

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

