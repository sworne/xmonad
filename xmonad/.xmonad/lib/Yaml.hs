-- IMPORTS
{-# LANGUAGE OverloadedStrings #-}

import Data.Yaml
import Control.Applicative -- <$>, <*>
import Data.Maybe (fromJust)
import GHC.Generics
import Data.Aeson

import qualified Data.ByteString.Char8 as BS

-- YAML CONFIG

data MyConfig = MyConfig {
    bg :: BgConfig,
    colors :: ColorConfig,
    apps :: AppConfig
  } deriving (Show)

data BgConfig = BgConfig {
    color :: String
  } deriving (Show)

data ColorConfig = ColorConfig {
    black :: String,
    white :: String,
    red :: String,
    green :: String,
    yellow :: String,
    blue :: String,
    magenta :: String,
    cyan :: String
} deriving (Show)

data AppConfig = AppConfig {
    term :: String,
    music :: String,
    web :: String,
    lock :: String,
    compositor :: String,
    launcher :: String
} deriving (Show)

-- YAML PARSE

instance FromJSON MyConfig where
    parseJSON (Object v) = MyConfig <$>
        v .: "bg" <*>
        v .: "colors" <*>
        v .: "apps"
    parseJSON x = fail ("not an object: " ++ show x)       

instance FromJSON BgConfig where
    parseJSON (Object v) = BgConfig <$>
        v .: "color"
    parseJSON x = fail ("not an object: " ++ show x)

instance FromJSON ColorConfig where
    parseJSON (Object v) = ColorConfig <$>
        v .: "black" <*>
        v .: "white" <*>
        v .: "red" <*>
        v .: "green" <*>
        v .: "yellow" <*>
        v .: "blue" <*>
        v .: "magenta" <*>
        v .: "cyan"
    parseJSON x = fail ("not an object: " ++ show x)

instance FromJSON AppConfig where
    parseJSON (Object v) = AppConfig <$>
        v .: "term" <*>
        v .: "music" <*>
        v .: "web" <*>
        v .: "lock" <*>
        v .: "compositor" <*>
        v .: "launcher"
    parseJSON x = fail ("not an object: " ++ show x)

envYAML :: FilePath
envYAML = "env.yml"
main = do
    content <- BS.readFile envYAML
    let config = Data.Yaml.decode content :: Maybe MyConfig
    print $ fromJust config