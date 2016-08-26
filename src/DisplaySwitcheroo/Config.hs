{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module DisplaySwitcheroo.Config
    ( Config(..)
    , DesiredSetup(..)
    , loadConfig
    ) where

import Data.Aeson
import Data.Aeson.Types ( typeMismatch )
import GHC.Generics
import qualified Data.ByteString.Lazy as B

data Config = Config { configDesiredSetups :: [DesiredSetup]
                     , configPreSwitchHooks :: [String]
                     , configPostSwitchHooks :: [String]
                     }
                     deriving ( Show, Generic )

instance FromJSON Config where
    parseJSON (Object v) = Config
        <$> v .: "setups"
        <*> (v .:? "preSwitchHooks" .!= [])
        <*> (v .:? "postSwitchHooks" .!= [])
    parseJSON invalid = typeMismatch "Config" invalid

data DesiredSetup = DesiredSetup { desiredSetupName :: Maybe String
                                 , desiredSetupOutputs :: [String]
                                 }
                                 deriving Show

instance FromJSON DesiredSetup where
    parseJSON (Object v) = DesiredSetup <$> v .:? "name" <*> v .: "outputs"
    parseJSON invalid = typeMismatch "DesiredSetup" invalid

loadConfig :: FilePath -> IO (Either String Config)
loadConfig path = do
    raw <- B.readFile path
    return $ eitherDecode raw
