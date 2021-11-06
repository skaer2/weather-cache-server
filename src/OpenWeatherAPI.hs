{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OpenWeatherAPI where

import Prelude hiding (lookup)

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant
import Servant.API
import Servant.Client
import Data.Text (Text)
import Data.Cache
import Data.Cache.Internal
import System.Clock

import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import qualified Servant.Client.Streaming as S

import OpenWeatherResponce

type OpenWeatherAPI = 
    "weather" :> QueryParam "q" City :> QueryParam "appid" OpenWeatherAPIKey :> QueryParam "units" Text :> Get '[JSON] OpenWeatherResponce

api :: Proxy OpenWeatherAPI
api = Proxy

openweatherQueryHandler :: Maybe City -> Maybe OpenWeatherAPIKey -> Maybe Text -> ClientM OpenWeatherResponce
openweatherQueryHandler = client api 

--(BaseUrl Http "api.openweathermap.org" 80 "/data/2.5/")
makeAPIQuery :: WeatherCache -> OpenWeatherAPIKey -> Int -> City -> Maybe Int -> IO (Maybe OpenWeatherResponce)
makeAPIQuery cache apikey precision city maybeDatetime = do
    case maybeDatetime of
      Nothing -> do
          maybeRes <- getCurrentWeatherResponce city apikey
          case maybeRes of
              Just res -> insert cache (name res, dt res) res
          return maybeRes
      Just datetime -> do
          ckeys <- keys cache
          let closestMatch = last $ filter (isInRange datetime) ckeys
          res <- lookup cache (city, snd closestMatch)
          return res
    where
        isInRange :: Int -> (City, Int) -> Bool
        isInRange dt (c, key) = (c == city) && key > (dt - precision `div` 2) && key <= (dt + precision `div` 2)

          
getCurrentWeatherResponce :: City -> OpenWeatherAPIKey -> IO (Maybe OpenWeatherResponce)
getCurrentWeatherResponce city apikey = do
    manager' <- newManager defaultManagerSettings
    res <- runClientM ( openweatherQueryHandler (Just city) (Just apikey) (Just "metric") ) (mkClientEnv manager' (BaseUrl Http "api.openweathermap.org" 80 "/data/2.5/"))
    case res of 
        Left err -> do
            putStrLn $ "Error: " ++ take 2000 (show err)
            return Nothing
        Right weatherResponce -> do
            --putStrLn $ show weatherResponce
            return (Just weatherResponce)
