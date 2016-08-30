module DisplaySwitcheroo.Logging
    ( infoL
    , debugL
    , warningL
    , errorL
    , setupLogging
    ) where

import DisplaySwitcheroo.Config
import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import Control.Monad.IO.Class
import System.IO ( stderr )

loggerName :: String
loggerName = "displayswitcheroo"

infoL :: MonadIO m => String -> m ()
infoL = liftIO . infoM loggerName

debugL :: MonadIO m => String -> m ()
debugL = liftIO . debugM loggerName

warningL :: MonadIO m => String -> m ()
warningL = liftIO . warningM loggerName

errorL :: MonadIO m => String -> m ()
errorL = liftIO . errorM loggerName

levelToPriority :: LoggingLevel -> Maybe Priority
levelToPriority LoggingLevelDebug = Just DEBUG
levelToPriority LoggingLevelInfo = Just INFO
levelToPriority LoggingLevelWarning = Just WARNING
levelToPriority LoggingLevelError = Just ERROR
levelToPriority LoggingLevelMute = Nothing


setupLogging :: MonadIO m => Config -> m ()
setupLogging Config { configLogging = LoggingConfig { loggingConfigStderr = stderrLevel, loggingConfigSyslog = syslogLevel } } = do
    liftIO $ updateGlobalLogger rootLoggerName removeHandler
    liftIO $ updateGlobalLogger loggerName (setLevel DEBUG)

    case levelToPriority syslogLevel of
      Just p -> do
          s <- liftIO $ openlog loggerName [PID] USER p
          liftIO $ updateGlobalLogger loggerName (addHandler s)
      Nothing -> return ()

    case levelToPriority stderrLevel of
      Just p -> do
          h <- liftIO $ streamHandler stderr p
          liftIO $ updateGlobalLogger loggerName (addHandler h)
      Nothing -> return ()
