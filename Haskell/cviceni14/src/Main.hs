{-# LANGUAGE RecordWildCards #-}
-- | Main module.
module Main
    ( main
    ) where

import Codec.Picture
import Options.Applicative

import Config
import Mandelbrot

color :: Int -> Pixel8
color = fromIntegral

opts :: ParserInfo Config
opts = info (parseConfig <* helper) (progDesc "Make a Mandelbrot set image.")

main :: IO ()
main = do
    c@Config{..} <- execParser opts
    let picture = generateImage (\x y -> color . converge limit $ pixelToComplex c x y) width height
    writePng output picture
