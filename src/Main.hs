{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import GHC.Int (Int64)
import System.Environment
import Control.Concurrent
import Control.Monad
import Data.Aeson
import GHC.Generics
import Network.Wai.Handler.Warp
import Data.Text (Text)
import Data.Cache
import System.Clock
import Data.Yaml

import qualified Data.Text                as T

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


delayedCaching :: WeatherCache -> OpenWeatherAPIKey -> APIDomain -> Int -> [City] -> Int -> IO ()
delayedCaching cache apikey apidomain precision cityList delay = do
    putStrLn "updating cache"
    _ <- mapM (\city -> makeAPIQuery cache apikey apidomain precision city Nothing) cityList
    threadDelay(microsecondsToMinutes delay)
    where
        microsecondsToMinutes :: Int -> Int
        microsecondsToMinutes x = 60 * 1000000 * x

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
            apiDomainName <- getEnv "API_DOMAIN_NAME"
            putStrLn "starting automatic cache updates"
            _ <- forkIO $ forever $ delayedCaching cache (T.pack apikey) apiDomainName (precisionRange conf) (cities conf) (updatesInterval conf)
            putStrLn "starting query listening"
            run (port conf) $ app1 cache (T.pack apikey) apiDomainName (precisionRange conf)

    where
        getConfig :: IO (Either ParseException Config)
        getConfig = decodeFileEither "config.yaml"
