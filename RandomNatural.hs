module RandomNatural
         (
         integerToNatural,
         integralToNatural,
         randomR,
         random
         ) where

import Numeric.Natural
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
integralToNatural :: Integral i => i -> Natural
integralToNatural = integerToNatural . toInteger

instance Random Natural where
  randomR (a,b) g = (nat,g') where
    (int,g') = randomIvalIntegral (a::Natural, b) g
    nat = integralToNatural int
    
  random = randomR (0, (integralToNatural (maxBound::Int)))
