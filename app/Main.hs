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
                [] -> infoL $ "No changes required"
                _ -> do
                    infoL $ "Selecting: " ++ show difference
                    _ <- get >>= applyChanges display root res initialSetup
                    debugL $ "Successfully applied changes"
          return ()
      Nothing -> do
          errorL $ "No desired setups present: " ++ show desiredSetups
