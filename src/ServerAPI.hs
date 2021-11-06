{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ServerAPI where

import Control.Monad.IO.Class
import Data.Proxy
import Servant

import OpenWeatherResponce
import OpenWeatherAPI

-- /weather/London?datetime=1241241241
type ServerAPI = 
    "weather" :> Capture "city" City :> QueryParam "datetime" Int :> Get '[JSON] OpenWeatherResponce

server1 :: WeatherCache -> OpenWeatherAPIKey -> APIDomain -> Int -> Server ServerAPI
server1 cache apikey apidomain precision = responceHandler cache apikey apidomain precision 

serverAPI :: Proxy ServerAPI
serverAPI = Proxy

app1 :: WeatherCache -> OpenWeatherAPIKey -> APIDomain -> Int -> Application
app1 cache apikey apidomain precision = serve serverAPI $ server1 cache apikey apidomain precision

responceHandler :: WeatherCache -> OpenWeatherAPIKey -> APIDomain -> Int -> City -> Maybe Int -> Handler OpenWeatherResponce
responceHandler cache apikey apidomain precision city dt = do
    responce <- liftIO $ makeAPIQuery cache apikey apidomain precision city dt 
    case responce of 
        Just res -> return res
        Nothing -> do
            throwError $ err404 { errBody = "Erorr 404" }
