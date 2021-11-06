{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ServerAPI where

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
import OpenWeatherAPI

-- /weather/London?datetime=1241241241
type ServerAPI = 
    "weather" :> Capture "city" City :> QueryParam "datetime" Int :> Get '[JSON] OpenWeatherResponce

server1 :: WeatherCache -> OpenWeatherAPIKey -> Int -> Server ServerAPI
server1 cache apikey precision = responceHandler cache apikey precision 

serverAPI :: Proxy ServerAPI
serverAPI = Proxy

app1 :: WeatherCache -> OpenWeatherAPIKey -> Int -> Application
app1 cache apikey precision = serve serverAPI $ server1 cache apikey precision

responceHandler :: WeatherCache -> OpenWeatherAPIKey -> Int -> City -> Maybe Int -> Handler OpenWeatherResponce
responceHandler cache apikey precision city dt = do
    responce <- liftIO $ makeAPIQuery cache apikey precision city dt 
    case responce of 
        Just res -> return res
        Nothing -> do
            throwError $ err404 { errBody = "Erorr 404" }
