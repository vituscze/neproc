{-# LANGUAGE RecordWildCards #-}
-- | Convergence testing.
module Mandelbrot
    ( converge
    , pixelToComplex
    ) where

import Data.Complex

import Config

-- | Test whether a complex number belongs to the Mandelbrot set.
--
-- The result is a number of iterations required for divergence to be
-- established, which happens when the number's absolute value is greater
-- than 2.
--
-- >>> converge 100 (1 :+ 0)
-- 3
--
converge :: Int  -- ^ Limit.
         -> Complex Double  -- ^ Initial point.
         -> Int
converge lim c = go 0 0
  where
    go steps z@(x :+ y)
        | steps == lim = lim
        | absSq <= 4   = go (steps + 1) (z * z + c)
        | otherwise    = steps
      where
        absSq = x * x + y * y

-- | Find a complex number corresponding to the given pixel coordinates.
--
-- Use with 'converge'.
pixelToComplex :: Config -> Int -> Int -> Complex Double
pixelToComplex Config{..} x y =
    (cx + xsteps * step) :+ (cy + ysteps * step)
  where
    cx :+ cy = center

    xsteps = fromIntegral $ x - width  `div` 2
    ysteps = fromIntegral $ y - height `div` 2
