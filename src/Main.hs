{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Prelude hiding (lookup)

import System.Environment
import Control.Concurrent
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
import Data.Yaml

import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import qualified Servant.Client.Streaming as S

import OpenWeatherResponce
import ServerAPI
import OpenWeatherAPI

data Config = Config
    { port :: Int
    , cities :: [Text]
    , updatesInterval :: Int
    , precisionRange :: Int
    } deriving (Eq, Show, Generic)

instance FromJSON Config where
    parseJSON = withObject "config" $ \o -> do
        port <- o .: "port"
        cities <- o .: "cities"
        updatesInterval <- o .:? "updatesInterval" .!= 60 -- update each hour
        precisionRange <- o .:? "precisionRange" .!= 3600 -- range is 30 minutes before and after the requested time
        return Config {..}



generateCache :: IO WeatherCache
generateCache = newCache $ Just $ TimeSpec {sec = 1800, nsec = 0}
  --newCache $ Just $ TimeSpec {sec = 604800, nsec = 0}


delayedCaching :: WeatherCache -> OpenWeatherAPIKey -> Int -> [City] -> Int -> IO ()
delayedCaching cache apikey precision cityList delay = do
    putStrLn "HELLO!!! :-)"
    mapM (\city -> makeAPIQuery cache apikey precision city Nothing) cityList
    threadDelay(10^6 * 60 * delay)

main :: IO ()
main = do
    cache <- generateCache
    insert cache ("London", 631) testResponce  
    putStrLn "entered main"
    config <- getConfig
    case config of 
        Left err -> putStrLn $ "Error" ++ (show err)
        Right conf -> do
            print conf
            apikey <- getEnv "API_KEY"
            forkIO $ forever $ delayedCaching cache (T.pack apikey) (precisionRange conf) (cities conf) (updatesInterval conf)
            run (port conf) $ app1 cache (T.pack apikey) (precisionRange conf)

    where
        getConfig :: IO (Either ParseException Config)
        getConfig = decodeFileEither "config.yaml"

testResponce :: OpenWeatherResponce
testResponce = OpenWeatherResponce 
    { coord = Coord
        { lat = 51.5085 
        , lon = -0.1257
        }
    , weather = []
    , responce_main = Responce_Main 
        { temp = 8.92
        , feels_like = 0
        , temp_min = 0
        , temp_max = 0
        , pressure = 0
        , humidity = 0
        , sea_level = Nothing
        , grnd_level = Nothing
        }
    , visibility = 10000
    , wind = Wind 
        {deg = 321
        , speed = 0.45
        , gust = Nothing
        }
    , clouds = Clouds {all = 90}
    , rain = Nothing
    , snow = Nothing
    , dt = 631
    , timezone = 0
    , id = 0
    , name = "London"
    }

