{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Prelude hiding (lookup)

import Data.Time.Calendar

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

data OpenWeatherResponce = OpenWeatherResponce 
    { coord :: Coord
    , weather :: [Weather]
    , responce_main :: Responce_Main
    , visibility :: Int
    , wind :: Wind
    , clouds :: Clouds
    , rain :: Maybe Rain
    , snow :: Maybe Snow
    , dt :: Int
    , timezone :: Int
    , id :: Int
    , name :: Text
    } deriving (Eq, Show, Generic)

instance FromJSON OpenWeatherResponce where
    parseJSON = withObject "OpenWeatherResponce" $ \o -> do
        coord <- o .: "coord"
        weather <- o .: "weather"
        responce_main <- o .: "main"
        visibility <- o .: "visibility"
        wind <- o .: "wind"
        clouds <- o .: "clouds"
        rain <- o .:? "rain"
        snow <- o .:? "snow"
        dt <- o .: "dt"
        timezone <- o .: "timezone"
        id <- o .: "id"
        name <- o .: "name"
        return OpenWeatherResponce {..}

instance ToJSON OpenWeatherResponce

data Coord = Coord 
    { lon :: Float
    , lat :: Float
    } deriving (Eq, Show, Generic)

instance FromJSON Coord

instance ToJSON Coord

data Weather = Weather
    { weather_id :: Int
    , weather_main :: Text
    , description :: Text
    , icon :: Text
    } deriving (Eq, Show, Generic)

instance FromJSON Weather where
    parseJSON = withObject "weather" $ \o -> do
        weather_id <- o .: "id"
        weather_main <- o .: "main"
        description <- o .: "description"
        icon <- o .: "icon"
        return Weather {..}

instance ToJSON Weather

data Responce_Main = Responce_Main
    { temp :: Float
    , feels_like :: Float
    , temp_min :: Float
    , temp_max :: Float
    , pressure :: Int
    , humidity :: Int
    , sea_level :: Maybe Int
    , grnd_level :: Maybe Int
    } deriving (Eq, Show, Generic)

instance FromJSON Responce_Main

instance ToJSON Responce_Main

data Wind = Wind
    { speed :: Float
    , deg :: Int
    , gust :: Maybe Float
    } deriving (Eq, Show, Generic)

instance FromJSON Wind

instance ToJSON Wind

data Clouds = Clouds { all :: Int }
    deriving (Eq, Show, Generic)

instance FromJSON Clouds

instance ToJSON Clouds

data Rain = Rain 
    { rain1h :: Float
    , rain3h :: Maybe Float
    } deriving (Eq, Show, Generic)

instance FromJSON Rain where
    parseJSON = withObject "rain" $ \o -> do
        rain1h <- o .: "1h"
        rain3h <- o .:? "3h"
        return Rain{..}

instance ToJSON Rain

data Snow = Snow 
    { snow1h :: Float
    , snow3h :: Maybe Float
    } deriving (Eq, Show, Generic)

instance FromJSON Snow where
    parseJSON = withObject "snow" $ \o -> do
        snow1h <- o .: "1h"
        snow3h <- o .:? "3h"
        return Snow{..}

instance ToJSON Snow

type City = Text

type OpenWeatherAPIKey = Text

type OpenWeatherAPI = 
    "weather" :> QueryParam "q" City :> QueryParam "appid" OpenWeatherAPIKey :> QueryParam "units" Text :> Get '[JSON] OpenWeatherResponce

api :: Proxy OpenWeatherAPI
api = Proxy

getWeatherResponce :: Maybe City -> Maybe OpenWeatherAPIKey -> Maybe Text -> ClientM OpenWeatherResponce
getWeatherResponce = client api 

-- /weather/London?datetime=1241241241
type ServerAPI = 
    "weather" :> Capture "city" City :> QueryParam "datetime" Int :> Get '[JSON] OpenWeatherResponce

server1 :: WeatherCache -> Server ServerAPI
server1 cache = responceHandler cache

serverAPI :: Proxy ServerAPI
serverAPI = Proxy

app1 :: WeatherCache -> Application
app1 cache = serve serverAPI $ server1 cache

responceHandler :: WeatherCache -> City -> Maybe Int -> Handler OpenWeatherResponce
responceHandler cache city dt = do
    responce <- liftIO $ makeAPIQuery cache city dt "d54ab34d48b6427d272b6a24f9194e7d"
    case responce of 
        Just res -> return res
        Nothing -> do
            liftIO $ putStrLn "throwing error"
            throwError $ err404 { errBody = "Erorr 404" }

--(BaseUrl Http "api.openweathermap.org" 80 "/data/2.5/")
makeAPIQuery :: WeatherCache -> City -> Maybe Int -> OpenWeatherAPIKey -> IO (Maybe OpenWeatherResponce)
makeAPIQuery cache city maybeDatetime apikey = do
    putStrLn "makeAPIQuery"
    case maybeDatetime of
      Nothing -> do
          putStrLn "datetime is Nothing"
          maybeRes <- getCurrentWeatherResponce city apikey
          case maybeRes of
              Just res -> insert cache (name res, dt res) res
          return maybeRes
      Just dt -> do
          ckeys <- keys cache
          let closestMatch = last $ filter (\(c, x) -> c == city && x > dt - 30 && x <= dt + 30) ckeys
          res <- lookup cache (city, snd closestMatch)
          return res
          
getCurrentWeatherResponce :: City -> OpenWeatherAPIKey -> IO (Maybe OpenWeatherResponce)
getCurrentWeatherResponce city apikey = do
    manager' <- newManager defaultManagerSettings
    res <- runClientM ( getWeatherResponce (Just city) (Just apikey) (Just "metric") ) (mkClientEnv manager' (BaseUrl Http "api.openweathermap.org" 80 "/data/2.5/"))
    case res of 
        Left err -> do
            putStrLn $ "Error: " ++ take 2000 (show err)
            return Nothing
        Right weatherResponce -> do
            putStrLn $ show weatherResponce
            return (Just weatherResponce)

type WeatherCache = Cache (City, Int) OpenWeatherResponce

generateCache :: IO WeatherCache
generateCache = newCache $ Just $ TimeSpec {sec = 1800, nsec = 0}
  --newCache $ Just $ TimeSpec {sec = 604800, nsec = 0}

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

main :: IO ()
main = do
    cache <- generateCache
    insert cache ("London", 631) testResponce  
    putStrLn "entered main"
    runEnv 8080 $ app1 cache

main2 :: IO ()
main2 = do
    cache <- generateCache
    manager' <- newManager defaultManagerSettings
    res <- runClientM (getWeatherResponce (Just "London") (Just "d54ab34d48b6427d272b6a24f9194e7d") (Just "metric") ) (mkClientEnv manager' (BaseUrl Http "api.openweathermap.org" 80 "/data/2.5/"))
    case res of
        Left err -> putStrLn $ "Error: " ++ take 2000 (show err)
        Right weatherResponce -> do
            putStrLn $ show weatherResponce
            insert cache ("London", dt weatherResponce) weatherResponce  
            putStrLn "\n"
            y <- lookup cache ("London", dt weatherResponce)
            case y of
                Nothing -> putStrLn $ "Nope"
                Just x -> putStrLn $ show x

type UserAPI1 = "users" :> Get '[JSON] [User]

data User = User
  { hername :: String
  , age :: Int
  , email :: String
  , registration_date :: Day
  } deriving (Eq, Show, Generic)

instance ToJSON User

users1 :: [User]
users1 =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
  , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
  ]

server2 :: Server UserAPI1
server2 = return users1

userAPI :: Proxy UserAPI1
userAPI = Proxy

app2 = serve userAPI server2


