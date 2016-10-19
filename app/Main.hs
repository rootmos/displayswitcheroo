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

configFilePath :: IO FilePath
configFilePath = getAppUserDataDirectory "config/displayswitcheroo.json"

run :: Config -> Xlib.Display -> X.Window -> IO ()
run config display root = do
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

data Opts = MkOpts { runAsDaemon :: Bool }

optsParser :: Parser Opts
optsParser = MkOpts <$> switch ( long "daemon" <> help "Run as a daemon" )

main :: IO ()
main = do
    opts <- execParser (info optsParser idm)

    Right config <- configFilePath >>= loadConfig
    setupLogging config

    display <- openDisplay ""
    root <- rootWindow display (defaultScreen display)

    case runAsDaemon opts of
      False -> run config display root
      True -> forever $ do
          run config display root
          threadDelay (3*10^6)
