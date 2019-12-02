{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Unsplash (
  UnsplashConfig (..),
  def,
  setWallpaper,
  updateWallpaper,
  wallPrompt,
  getKey) where

import Control.Lens hiding ((??), elements)
import Data.Aeson hiding (Options)
import Data.Aeson.Lens
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as B
import Network.Wreq
import Test.QuickCheck (generate, elements)
import XMonad.Prompt
import XMonad (spawn, liftIO, X)
import Control.Concurrent.Async
import Data.Maybe
import Control.Monad.IO.Class
import Data.Default
import Data.List.Split
import System.IO
import Control.Exception
import System.Directory

data Wall = Wall
instance XPrompt Wall where
  showXPrompt Wall = "Wallpaper: "

data UnsplashConfig =
  UnsplashConfig {
      url      :: String
    , filename :: String
    , apiKey   :: IO String
    , command  :: String
    , queries  :: [String]
  }

instance Default UnsplashConfig where
  def = 
    UnsplashConfig {
        url = "https://api.unsplash.com/photos/random?query="
      , filename = "/var/tmp/bg"
      , apiKey = getKey keyPath
      , command = "feh --bg-fill"
      , queries = ["wallpaper"]
    }

data ImageDetails =
  ImageDetails {
      raw :: String
    , html :: String
  }

keyPath :: IO String
keyPath = do
  h <- getHomeDirectory
  return (h ++ "/.unsplash-key")

getKey :: IO String -> IO String
getKey f = do
  fp <- f
  str <- readKeyFile fp
  return $ checkKey str

readKeyFile :: String -> IO String
readKeyFile f = (catch :: IO a -> (IOError -> IO a) -> IO a) (readFile f) (\_ -> return "")

checkKey :: String -> String
checkKey s | length (words s) == 1 = (words s) !! 0 | otherwise = ""

generateCMD :: String -> String -> String
generateCMD s ss = s ++ " " ++ ss

wallPrompt :: XPConfig -> UnsplashConfig -> X ()
wallPrompt xpc config = do
  mkXPrompt Wall xpc (promptCompl config) (promptAsync config)

promptCompl :: UnsplashConfig -> String -> IO [String]
promptCompl (UnsplashConfig { queries = queries}) = mkComplFunFromList queries

getWallpaper :: String -> UnsplashConfig -> IO ()
getWallpaper query (UnsplashConfig { .. }) = do
  key <- liftIO apiKey
  link <- unsplash key query url
  download (T.unpack link) filename

getRandomWallpaper :: UnsplashConfig -> UnsplashConfig -> IO ()
getRandomWallpaper config (UnsplashConfig { .. }) = do
  query <- randItem queries
  getWallpaper query config

setWallpaper :: UnsplashConfig -> X ()
setWallpaper config = do
  cmd config
  liftIO $ async $ getRandomWallpaper config config
  cmd config

promptAsync :: UnsplashConfig -> String -> X ()
promptAsync config query = do
  liftIO $ async $ updateWallpaper config query
  cmd config

updateWallpaper :: UnsplashConfig -> String -> IO () 
updateWallpaper config query = do
  getWallpaper query config

cmd :: UnsplashConfig -> X ()
cmd (UnsplashConfig { .. }) = do
  spawn c where
    c = command ++ " " ++ filename

opts :: String -> Options
opts myID = defaults & header "Authorization" .~ [BS.pack $ "Client-ID " ++ myID]

unsplash :: String -> String -> String -> IO T.Text
unsplash uKey query url = do
    r <- getWith (opts uKey) (url ++ query)
    return $ r ^. responseBody . key "urls" . key "raw" . _String

download :: String -> String -> IO ()
download url filename = do
  r <- get url
  B.writeFile filename (r ^. responseBody)

randItem :: [a] -> IO a
randItem = generate . elements
