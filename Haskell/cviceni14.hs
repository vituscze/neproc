{-# LANGUAGE RecordWildCards #-}

import Data.Complex
import System.Environment

import Codec.Picture


converge :: Int
         -> Complex Double
         -> Int
converge lim c = go 0 0
  where
    go steps z@(x :+ y)
      | steps == lim = lim
      | absSq <= 4   = go (steps + 1) (z ^ 2 + c)
      | otherwise    = steps
      where
        absSq = x * x + y * y

data Config
    = Config
    { width  :: Int
    , height :: Int
    , center :: Complex Double
    , step   :: Double
    , limit  :: Int
    }

pixelToComplex :: Config -> Int -> Int -> Complex Double
pixelToComplex Config{..} x y =
    (cx + xsteps * step) :+ (cy + ysteps * step)
  where
    cx :+ cy = center

    xsteps = fromIntegral $ x - width  `div` 2
    ysteps = fromIntegral $ y - height `div` 2

color :: Int -> Pixel8
color = fromIntegral

main = do
    -- width height x y pixelSize limit outputName
    [w, h, cx, cy, s, l, out] <- getArgs
    let c = Config (read w) (read h) (read cx :+ read cy) (read s) (read l)
        picture = generateImage
            (\x y -> color . converge (limit c) $ pixelToComplex c x y)
            (width c) (height c)
    writePng out picture
