module Main where

import DisplaySwitcheroo
import DisplaySwitcheroo.Config
import DisplaySwitcheroo.Logging

import Graphics.X11 ( openDisplay
                    , rootWindow
                    , defaultScreen
                    )
import Graphics.X11.Xrandr
import Data.Either ( rights )
import Data.Maybe ( listToMaybe )
import Control.Monad.State.Strict
import Control.Monad.Except
import System.Directory ( getAppUserDataDirectory )
import Text.Printf
import System.Process

configFilePath :: IO FilePath
configFilePath = getAppUserDataDirectory "config/displayswitcheroo.json"

main :: IO ()
main = do
    Right config <- configFilePath >>= loadConfig

    display <- openDisplay ""
    root <- rootWindow display (defaultScreen display)
    Just res <- xrrGetScreenResourcesCurrent display root
    initialSetup <- fetchSetup display res

    setupLogging config

    let desiredSetups = map (compareDesiredSetup initialSetup) (configDesiredSetups config)
        selectedSetup = listToMaybe . rights $ desiredSetups

    case selectedSetup of
      Just difference -> do
          _ <- (flip runStateT) initialSetup . runExceptT $ do
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
          return ()
      Nothing -> do
          errorL $ "No desired setups present: " ++ show desiredSetups

runHook :: String -> IO ()
runHook cmdline = do
    debugL $ printf "Running hook: [%s]" cmdline
    (exitCode, stdoutContent, stderrContent) <- readCreateProcessWithExitCode (shell cmdline) ""
    infoL $ printf "Ran hook: [%s] ExitCode: %s StdOut: [%s] StdErr: [%s]" cmdline (show exitCode) stdoutContent stderrContent

