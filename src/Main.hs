{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Prelude hiding (lookup)

import GHC.Int (Int64)
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
    , updatesInterval :: Int --time interval in minutes
    , precisionRange :: Int --range in seconds
    , cacheExpirationTime :: Int64 --time cache is stored for in seconds
    } deriving (Eq, Show, Generic)

instance FromJSON Config where
    parseJSON = withObject "config" $ \o -> do
        port <- o .: "port"
        cities <- o .: "cities"
        updatesInterval <- o .:? "updatesInterval" .!= 60 
        precisionRange <- o .:? "precisionRange" .!= 3600         
        cacheExpirationTime <- o .:? "cacheExpirationTime" .!= 1800 
        return Config {..}



generateCache :: Int64 -> IO WeatherCache
generateCache expirationTime = newCache $ Just $ TimeSpec {sec = expirationTime, nsec = 0}


delayedCaching :: WeatherCache -> OpenWeatherAPIKey -> Int -> [City] -> Int -> IO ()
delayedCaching cache apikey precision cityList delay = do
    putStrLn "updating cache"
    mapM (\city -> makeAPIQuery cache apikey precision city Nothing) cityList
    threadDelay(10^6 * 60 * delay)

main :: IO ()
main = do
    putStrLn "starting the server"
    config <- getConfig
    putStrLn "attempting to read config.yaml"
    case config of 
        Left err -> putStrLn $ "Error" ++ (show err)
        Right conf -> do
            putStrLn $ "current config is:"
            putStrLn $ show conf
            cache <- generateCache (cacheExpirationTime conf)
            insert cache ("London", 631) testResponce  
            apikey <- getEnv "API_KEY"
            --apiDomainName <- getEnv "API_DOMAIN_NAME"
            putStrLn "starting automatic cache updates"
            forkIO $ forever $ delayedCaching cache (T.pack apikey) (precisionRange conf) (cities conf) (updatesInterval conf)
            putStrLn "starting query listening"
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

