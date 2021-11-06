{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OpenWeatherResponce where

import Data.Aeson
import GHC.Generics
import Data.Text (Text)
import Data.Cache

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
    , responce_id :: Int
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
        responce_id <- o .: "id"
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

type APIDomain = String

type WeatherCache = Cache (City, Int) OpenWeatherResponce
