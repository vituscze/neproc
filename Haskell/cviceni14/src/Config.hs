-- | Image generation configuration.
module Config
    ( -- * The @Config@ type
      Config(..)

      -- * Option parsers
    , parseConfig
    ) where

import Data.Complex
import Options.Applicative

data Config
    = Config
    { width  :: Int
    , height :: Int
    , center :: Complex Double
    , step   :: Double
    , limit  :: Int
    , output :: String
    }

parseConfig :: Parser Config
parseConfig =
    Config <$> argument auto (metavar "WIDTH")
           <*> argument auto (metavar "HEIGHT")
           <*> parseCenter
           <*> option auto (short 's' <> long "step"   <> value 0.01)
           <*> option auto (short 'l' <> long "limit"  <> value 255)
           <*> strOption   (short 'o' <> long "output" <> value "img.png")
  where
    parseCenter = (:+) <$> argument auto (metavar "CENTERX")
                       <*> argument auto (metavar "CENTERY")
