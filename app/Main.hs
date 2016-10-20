{-# LANGUAGE OverloadedStrings #-}
module Main where

import DisplaySwitcheroo
import DisplaySwitcheroo.Config
import DisplaySwitcheroo.Logging

import Graphics.X11 ( openDisplay
                    , rootWindow
                    , defaultScreen
                    )
import Graphics.X11.Xrandr
import qualified Graphics.X11.Types as X
import qualified Graphics.X11.Xlib.Types as Xlib
import Data.Either ( rights )
import Data.Maybe ( listToMaybe )
import Control.Monad.State.Strict
import System.Directory ( getAppUserDataDirectory )
import Text.Printf
import System.Process
import Options.Applicative
import Control.Concurrent ( threadDelay )
import qualified Data.Map.Strict as M
import Data.Aeson
import Data.Aeson.Encode.Pretty ( encodePretty )
import qualified Data.ByteString.Lazy.Char8 as B8

configFilePath :: IO FilePath
configFilePath = getAppUserDataDirectory "config/displayswitcheroo.json"

data OutputInfo = MkOutputInfo { outputInfoName :: String
                               , outputInfoX :: Int
                               , outputInfoY :: Int
                               , outputInfoWidth :: Int
                               , outputInfoHeigth :: Int
                               , outputInfoRefreshRate :: Double
                               , outputInfoInterlaced :: Bool
                               , outputInfoDoubleScan :: Bool
                               , outputInfoDpiX :: Double
                               , outputInfoDpiY :: Double
                               }
                               deriving ( Show )

instance ToJSON OutputInfo where
    toJSON oi = object [ "name" .= outputInfoName oi
                       , "x" .= outputInfoX oi
                       , "y" .= outputInfoY oi
                       , "width" .= outputInfoWidth oi
                       , "height" .= outputInfoHeigth oi
                       , "dpiX" .= outputInfoDpiX oi
                       , "dpiY" .= outputInfoDpiY oi
                       , "refresh_rate" .= outputInfoRefreshRate oi
                       , "interlaced" .= outputInfoInterlaced oi
                       , "double_scan" .= outputInfoDoubleScan oi
                       ]

toOutputInfo :: Setup -> Output -> Maybe OutputInfo
toOutputInfo setup output = do
    monitor <- outputMonitor output >>= (flip lookupMonitor) setup
    mode <- monitorMode monitor
    return $ MkOutputInfo { outputInfoName = outputName output
                          , outputInfoX = monitorX monitor
                          , outputInfoY = monitorY monitor
                          , outputInfoWidth = monitorWidth monitor
                          , outputInfoHeigth = monitorHeight monitor
                          , outputInfoRefreshRate = modeHz mode
                          , outputInfoInterlaced = modeInterlaced mode
                          , outputInfoDoubleScan = modeDoubleScan mode
                          , outputInfoDpiX = dpiX monitor output
                          , outputInfoDpiY = dpiY monitor output
                          }

doInfo :: Config -> Xlib.Display -> X.Window -> IO ()
doInfo config display root = do
    Just res <- xrrGetScreenResourcesCurrent display root
    initialSetup <- fetchSetup display res
    let enabledOutputs = filter isOutputEnabled . M.elems $ setupOutputs initialSetup
    B8.putStrLn . encodePretty $ toJSON (map (toOutputInfo initialSetup) enabledOutputs)

doRun :: Config -> Xlib.Display -> X.Window -> IO ()
doRun config display root = do
    Just res <- xrrGetScreenResourcesCurrent display root
    initialSetup <- fetchSetup display res

    let desiredSetups = map (compareDesiredSetup initialSetup) (configDesiredSetups config)
        selectedSetup = listToMaybe . rights $ desiredSetups

    case selectedSetup of
      Just difference -> do
          (result, _) <- runDisplaySwitcheroo initialSetup $ do
              outputsToEnable <- sequence $ map lookupOutputE (setupDifferenceEnable difference)
              leftest <- topLeft $ head outputsToEnable
              foldM_ (\left right -> right `rightOfMonitor` left) leftest (tail outputsToEnable)

              outputsToDisable <- sequence $ map lookupOutputE (setupDifferenceDisable difference)
              mapM_ disable outputsToDisable

              newSetup <- get
              case (changes newSetup) of
                [] -> debugL $ "No changes required"
                _ -> do
                    infoL $ "Selecting: " ++ show difference
                    mapM_ (liftIO . runHook) (configPreSwitchHooks config)
                    _ <- get >>= applyChanges display root res initialSetup
                    infoL $ "Successfully applied changes"
                    mapM_ (liftIO . runHook) (configPostSwitchHooks config)

          case result of
            Left e -> errorL $ "Failed to do switcheroo: " ++ show e
            Right _ -> return ()
      Nothing -> do
          errorL $ "No desired setups present: " ++ show desiredSetups

runHook :: String -> IO ()
runHook cmdline = do
    debugL $ printf "Running hook: [%s]" cmdline
    (exitCode, stdoutContent, stderrContent) <- readCreateProcessWithExitCode (shell cmdline) ""
    infoL $ printf "Ran hook: [%s] ExitCode: %s StdOut: [%s] StdErr: [%s]" cmdline (show exitCode) stdoutContent stderrContent

data Opts = MkRunOpts { runAsDaemon :: Bool }
          | MkInfoOpts

runOptsParser :: Parser Opts
runOptsParser = MkRunOpts <$> switch ( long "daemon" <> help "Run as a daemon" )

infoOptsParser :: Parser Opts
infoOptsParser = subparser (command "info" (info (pure MkInfoOpts) idm))

optsParser :: Parser Opts
optsParser = infoOptsParser <|> runOptsParser

main :: IO ()
main = do
    opts <- execParser (info optsParser idm)

    Right config <- configFilePath >>= loadConfig
    setupLogging config

    display <- openDisplay ""
    root <- rootWindow display (defaultScreen display)

    case opts of
      MkRunOpts { runAsDaemon = False } -> doRun config display root
      MkRunOpts { runAsDaemon = True } -> forever $ do
          doRun config display root
          threadDelay (3*10^6)
      MkInfoOpts -> doInfo config display root

