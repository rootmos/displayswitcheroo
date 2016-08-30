{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module DisplaySwitcheroo.Config
    ( Config(..)
    , DesiredSetup(..)
    , LoggingLevel(..)
    , LoggingConfig(..)
    , loadConfig
    ) where

import Data.Aeson
import Data.Aeson.Types ( typeMismatch )
import qualified Data.ByteString.Lazy as B

data Config = Config { configDesiredSetups :: [DesiredSetup]
                     , configPreSwitchHooks :: [String]
                     , configPostSwitchHooks :: [String]
                     , configLogging :: LoggingConfig
                     }
                     deriving Show

instance FromJSON Config where
    parseJSON (Object v) = Config
        <$> v .: "setups"
        <*> (v .:? "preSwitchHooks" .!= [])
        <*> (v .:? "postSwitchHooks" .!= [])
        <*> (v .:? "logging" .!= defaultLoggingConfig)
    parseJSON invalid = typeMismatch "Config" invalid

data DesiredSetup = DesiredSetup { desiredSetupName :: Maybe String
                                 , desiredSetupOutputs :: [String]
                                 }
                                 deriving Show

instance FromJSON DesiredSetup where
    parseJSON (Object v) = DesiredSetup <$> v .:? "name" <*> v .: "outputs"
    parseJSON invalid = typeMismatch "DesiredSetup" invalid

data LoggingLevel = LoggingLevelDebug | LoggingLevelInfo | LoggingLevelWarning | LoggingLevelError | LoggingLevelMute
    deriving Show

instance FromJSON LoggingLevel where
    parseJSON "debug" = return LoggingLevelDebug
    parseJSON "info" = return LoggingLevelInfo
    parseJSON "warning" = return LoggingLevelWarning
    parseJSON "error" = return LoggingLevelError
    parseJSON "mute" = return LoggingLevelMute
    parseJSON invalid = typeMismatch "LoggingLevel" invalid

data LoggingConfig = LoggingConfig { loggingConfigStderr :: LoggingLevel
                                   , loggingConfigSyslog :: LoggingLevel
                                   }
                                   deriving Show

instance FromJSON LoggingConfig where
    parseJSON (Object v) = LoggingConfig <$> (v .:? "stderr" .!= defaultStderrLoggingLevel)
                                         <*> (v .:? "syslog" .!= defaultSyslogLoggingLevel)
    parseJSON invalid = typeMismatch "LoggingConfig" invalid

defaultStderrLoggingLevel :: LoggingLevel
defaultStderrLoggingLevel = LoggingLevelWarning

defaultSyslogLoggingLevel :: LoggingLevel
defaultSyslogLoggingLevel = LoggingLevelMute

defaultLoggingConfig :: LoggingConfig
defaultLoggingConfig = LoggingConfig defaultStderrLoggingLevel defaultSyslogLoggingLevel

loadConfig :: FilePath -> IO (Either String Config)
loadConfig path = do
    raw <- B.readFile path
    return $ eitherDecode raw
