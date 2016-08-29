{-# LANGUAGE FlexibleContexts #-}
module DisplaySwitcheroo
    ( doSwitcheroo
    ) where

import DisplaySwitcheroo.Config

import Graphics.X11 ( openDisplay
                    , rootWindow
                    , defaultScreen
                    )
import Graphics.X11.Xlib.Extras ( currentTime, none )
import Graphics.X11.Xrandr
import qualified Graphics.X11.Types as X
import qualified Graphics.X11.Xlib.Types as Xlib
import Control.Monad ( liftM
                     , forM
                     , forM_
                     )
import Data.Maybe ( catMaybes
                  , listToMaybe
                  )
import Data.List ( unzip4
                 , find
                 , intersect
                 , (\\)
                 , partition
                 )
import qualified Data.Map.Strict as M
import Data.Bits ( testBit )
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.Bifunctor ( bimap )
import Text.Printf ( printf )


newtype OutputId = OutputId X.RROutput
    deriving ( Show, Eq, Ord )

data Output = Output { outputId :: OutputId
                     , outputName :: String
                     , outputModes :: [Mode]
                     , outputMonitor :: Maybe MonitorId
                     , outputMonitors :: [MonitorId]
                     , outputWidthInMillimeters :: Int
                     , outputHeightInMillimeters :: Int
                     }
                     deriving Show

isOutputConnected :: Output -> Bool
isOutputConnected Output { outputModes = [] } = False
isOutputConnected _ = True

isOutputEnabled :: Output -> Bool
isOutputEnabled Output { outputMonitor = Nothing } = False
isOutputEnabled _ = True

preferredMode :: Output -> Maybe Mode
preferredMode Output { outputModes = [] } = Nothing
preferredMode Output { outputModes = modes } = Just $ maximum modes

preferredModeE :: MonadError Failure m => Output -> m Mode
preferredModeE output = maybe (throwError $ UnableToDeterminePreferredMode output) return $ preferredMode output

newtype MonitorId = MonitorId X.RRCrtc
    deriving ( Show, Eq, Ord )

data Monitor = Monitor { monitorId :: MonitorId
                       , monitorX :: Int
                       , monitorY :: Int
                       , monitorWidth :: Int
                       , monitorHeight :: Int
                       , monitorMode :: Maybe Mode
                       , monitorOutputs :: [OutputId]
                       , monitorChanged :: Bool
                       }
                       deriving Show

isMonitorEnabled :: Monitor -> Bool
isMonitorEnabled Monitor { monitorMode = Nothing } = False
isMonitorEnabled _ = True

newtype ModeId = ModeId X.RRMode
    deriving ( Show, Eq, Ord)

data Mode = Mode { modeId :: ModeId
                 , modeWidth :: Int
                 , modeHeight :: Int
                 , modeHz :: Double
                 , modeInterlaced :: Bool
                 , modeDoubleScan :: Bool
                 }
                 deriving ( Eq )

instance Ord Mode where
    x `compare` y = (toTuple x) `compare` (toTuple y)
        where toTuple m = (modeWidth m, modeHeight m, modeHz m, not . modeInterlaced $ m)

instance Show Mode where
    show mode = printf "%dx%d%s(%.2fHz)" (modeWidth mode) (modeHeight mode) flags (modeHz mode)
        where flags = if (modeInterlaced mode) then "i" else "" ++ if (modeDoubleScan mode) then "d" else ""

modeMap :: XRRScreenResources -> M.Map ModeId Mode
modeMap XRRScreenResources { xrr_sr_modes = modes } =
    M.fromAscList $ map modeMaker modes
        where
            modeMaker mi =
                (ModeId (xrr_mi_id mi), Mode { modeId = ModeId (xrr_mi_id mi)
                                             , modeWidth = fromIntegral $ xrr_mi_width mi
                                             , modeHeight = fromIntegral $ xrr_mi_height mi
                                             , modeHz = refreshRate mi
                                             , modeInterlaced = isInterlaced mi
                                             , modeDoubleScan = isDoubleScan mi
                                             })

refreshRate :: XRRModeInfo -> Double
refreshRate mi = dotClock / (hTotal * vTotal)
    where
        dotClock = fromIntegral $ xrr_mi_dotClock mi
        hTotal = fromIntegral $ xrr_mi_hTotal mi
        vTotal
          | (isDoubleScan mi) && not (isInterlaced mi) = 2 * (fromIntegral $ xrr_mi_vTotal mi)
          | not (isDoubleScan mi) && (isInterlaced mi) = (fromIntegral $ xrr_mi_vTotal mi) / 2
          | otherwise = fromIntegral $ xrr_mi_vTotal mi

isInterlaced :: XRRModeInfo -> Bool
isInterlaced mi = testBit (xrr_mi_modeFlags mi) 4

isDoubleScan :: XRRModeInfo -> Bool
isDoubleScan mi = testBit (xrr_mi_modeFlags mi) 5

data Setup = Setup { setupOutputs :: M.Map OutputId Output
                   , setupMonitors :: M.Map MonitorId Monitor
                   }
                   deriving Show

fetchSetup :: Xlib.Display -> XRRScreenResources -> IO Setup
fetchSetup display res = do
    rawOutputs <- rawOutputsM
    let outputs = M.mapWithKey outputMaker rawOutputs

    rawMonitors <- rawMonitorsM
    let monitors = M.mapWithKey monitorMaker rawMonitors

    return $ Setup { setupOutputs = outputs, setupMonitors = monitors }
        where
            rawOutputsM = liftM (M.fromAscList . catMaybes) $ forM (xrr_sr_outputs res) (\oid -> do
                maybeOi <- xrrGetOutputInfo display res oid
                return $ ((,) (OutputId oid)) <$> maybeOi)

            rawMonitorsM = liftM (M.fromAscList . catMaybes) $ forM (xrr_sr_crtcs res) (\cid -> do
                maybeCi <- xrrGetCrtcInfo display res cid
                return $ ((,) (MonitorId cid)) <$> maybeCi)

            outputMaker oid oi =
                Output { outputId = oid
                       , outputName = (xrr_oi_name oi)
                       , outputModes = catMaybes $ map (\m -> M.lookup (ModeId m) (modeMap res)) (xrr_oi_modes oi)
                       , outputMonitor = case (fromIntegral $ xrr_oi_crtc oi) of
                                           0 -> Nothing
                                           i -> Just (MonitorId i)
                       , outputMonitors = map (MonitorId . fromIntegral) (xrr_oi_crtcs oi)
                       , outputWidthInMillimeters = fromIntegral $ xrr_oi_mm_width oi
                       , outputHeightInMillimeters = fromIntegral $ xrr_oi_mm_height oi
                       }

            monitorMaker mid ci =
                Monitor { monitorId = mid
                        , monitorX = (fromIntegral $ xrr_ci_x ci)
                        , monitorY = (fromIntegral $ xrr_ci_y ci)
                        , monitorWidth = (fromIntegral $ xrr_ci_width ci)
                        , monitorHeight = (fromIntegral $ xrr_ci_height ci)
                        , monitorMode = M.lookup (ModeId $ xrr_ci_mode ci) (modeMap res)
                        , monitorOutputs = map (OutputId . fromIntegral) (xrr_ci_outputs ci)
                        , monitorChanged = False
                        }

updateMonitor :: MonadIO m => Xlib.Display -> XRRScreenResources -> Monitor -> m X.Status
updateMonitor display res (monitor @ Monitor { monitorId = MonitorId cid, monitorMode = Nothing }) =
    liftIO $ xrrSetCrtcConfig display res cid currentTime 0 0 none X.xRR_Rotate_0 []

updateMonitor display res (monitor @ Monitor { monitorId = MonitorId cid
                                             , monitorMode = Just (Mode { modeId = ModeId mid })
                                             }) = do
    Just prevConfig <- liftIO $ xrrGetCrtcInfo display res cid
    let x = fromIntegral $ monitorX monitor
        y = fromIntegral $ monitorY monitor
        rot = xrr_ci_rotations prevConfig
        outputs = map (\(OutputId i) -> i) $ monitorOutputs monitor

    liftIO $ xrrSetCrtcConfig display res cid currentTime x y mid X.xRR_Rotate_0 outputs

updateScreen :: MonadIO m => Xlib.Display -> X.Window -> (Int, Int, Int, Int) -> m ()
updateScreen display root (width, height, widthInMillimeters, heightInMillimeters) =
    liftIO $ xrrSetScreenSize display root (fromIntegral width) (fromIntegral height) (fromIntegral widthInMillimeters) (fromIntegral heightInMillimeters)

calculateScreenDimensions :: Setup -> (Int, Int, Int, Int)
calculateScreenDimensions Setup { setupOutputs = outputs, setupMonitors = monitors } =
    (maxX, maxY, maxXmm, maxYmm)
    where
        maxX = maximum maxXs
        maxY = maximum maxYs
        maxXmm = round $ (fromIntegral maxX) / denX
        maxYmm = round $ (fromIntegral maxY) / denY
        denX = maximum denXs
        denY = maximum denYs
        (maxXs, maxYs, denXs, denYs) = unzip4 . catMaybes $ map considerOutput (M.elems outputs)
        considerOutput :: Output -> Maybe (Int, Int, Float, Float)
        considerOutput output = do
            mid <- outputMonitor output
            monitor <- M.lookup mid monitors
            let mX = monitorX monitor + monitorWidth monitor
                mY = monitorY monitor + monitorHeight monitor
                dX = (fromIntegral $ monitorWidth monitor) / (fromIntegral $ outputWidthInMillimeters output)
                dY = (fromIntegral $ monitorHeight monitor) / (fromIntegral $ outputHeightInMillimeters output)
            return (mX, mY, dX, dY)

rightOfMonitor :: (MonadError Failure m, MonadState Setup m) => Output -> Monitor -> m Monitor
output `rightOfMonitor` existingMonitor = do
    mode <- preferredModeE output
    monitor <- case output of
      Output { outputMonitor = Nothing } -> freeMonitor output
      Output { outputMonitor = Just mid } -> lookupMonitorE mid

    let modifiedMonitor = monitor { monitorMode = Just mode
                                  , monitorX = monitorX existingMonitor + monitorWidth existingMonitor
                                  , monitorY = monitorY existingMonitor
                                  , monitorWidth = modeWidth mode
                                  , monitorHeight = modeHeight mode
                                  , monitorOutputs = [outputId output]
                                  , monitorChanged = True
                                  }
        modifiedOutput = output { outputMonitor = Just (monitorId modifiedMonitor) }
    modify $ upsertMonitor modifiedMonitor . upsertOutput modifiedOutput
    return modifiedMonitor

freeMonitor :: (MonadError Failure m, MonadState Setup m) => Output -> m Monitor
freeMonitor output = get >>= \Setup { setupMonitors = monitors } ->
    maybe (throwError NoFreeMonitors) return $
        listToMaybe . filter (not . isMonitorEnabled) . catMaybes $
            map (flip M.lookup $ monitors) (outputMonitors output)

rightOf :: (MonadError Failure m, MonadState Setup m) => Output -> Output -> m Monitor
output `rightOf` (existingOutput @ Output { outputMonitor = Nothing }) = throwError $ OutputNotEnabled existingOutput
output `rightOf` (Output { outputMonitor = Just mid }) = do
    monitor <- lookupMonitorE mid
    output `rightOfMonitor` monitor

topLeft :: (MonadError Failure m, MonadState Setup m) => Output -> m Monitor
topLeft output = do
    mode <- preferredModeE output
    monitor <- case output of
      Output { outputMonitor = Nothing } -> freeMonitor output
      Output { outputMonitor = Just mid } -> lookupMonitorE mid

    let modifiedMonitor = monitor { monitorMode = Just mode
                                  , monitorX = 0
                                  , monitorY = 0
                                  , monitorWidth = modeWidth mode
                                  , monitorHeight = modeHeight mode
                                  , monitorOutputs = [outputId output]
                                  , monitorChanged = True
                                  }
        modifiedOutput = output { outputMonitor = Just (monitorId modifiedMonitor) }

    modify $ upsertMonitor modifiedMonitor . upsertOutput modifiedOutput
    return modifiedMonitor

sameAsMonitor :: (MonadError Failure m, MonadState Setup m) => Output -> Monitor -> m Monitor
output `sameAsMonitor` existingMonitor = do
    mode <- preferredModeE output
    monitor <- case output of
      Output { outputMonitor = Nothing } -> freeMonitor output
      Output { outputMonitor = Just mid } -> lookupMonitorE mid

    let modifiedMonitor = monitor { monitorMode = Just mode
                                  , monitorX = monitorX existingMonitor
                                  , monitorY = monitorY existingMonitor
                                  , monitorWidth = modeWidth mode
                                  , monitorHeight = modeHeight mode
                                  , monitorOutputs = [outputId output]
                                  , monitorChanged = True
                                  }
        modifiedOutput = output { outputMonitor = Just (monitorId modifiedMonitor) }

    modify $ upsertMonitor modifiedMonitor . upsertOutput modifiedOutput
    return modifiedMonitor

sameAs :: (MonadError Failure m, MonadState Setup m) => Output -> Output -> m Monitor
output `sameAs` (existingOutput @ Output { outputMonitor = Nothing }) = throwError $ OutputNotEnabled existingOutput
output `sameAs` (Output { outputMonitor = Just mid }) = do
    monitor <- lookupMonitorE mid
    output `sameAsMonitor` monitor

disable :: (MonadError Failure m, MonadState Setup m) => Output -> m Monitor
disable o @ Output { outputMonitor = Nothing } = throwError $ OutputAlreadyDisabled o
disable output @ Output { outputMonitor = Just mid } = do
    monitor <- lookupMonitorE mid
    let newMonitor = monitor { monitorMode = Nothing }
        newOutput = output { outputMonitor = Nothing }
    modify $ upsertMonitor newMonitor . upsertOutput newOutput
    return newMonitor

upsertMonitor :: Monitor -> Setup -> Setup
upsertMonitor monitor setup = setup { setupMonitors = M.insert (monitorId monitor) monitor (setupMonitors setup) }

upsertOutput :: Output -> Setup -> Setup
upsertOutput output setup = setup { setupOutputs = M.insert (outputId output) output (setupOutputs setup) }

findOutput :: String -> Setup -> Maybe Output
findOutput name = find ((== name) . outputName) . setupOutputs

findOutputE :: (MonadError Failure m, MonadState Setup m) => String -> m Output
findOutputE name = (gets $ findOutput name) >>= maybe (throwError $ NoSuchOutput name) return

lookupMonitor :: MonitorId -> Setup -> Maybe Monitor
lookupMonitor id = M.lookup id . setupMonitors

lookupMonitorE :: (MonadError Failure m, MonadState Setup m) => MonitorId -> m Monitor
lookupMonitorE id = (gets $ lookupMonitor id) >>= maybe (throwError $ MonitorNotFound id) return

lookupOutput :: OutputId -> Setup -> Maybe Output
lookupOutput id = M.lookup id . setupOutputs

lookupOutputE :: (MonadError Failure m, MonadState Setup m) => OutputId -> m Output
lookupOutputE id = (gets $ lookupOutput id) >>= maybe (throwError $ OutputNotFound id) return

data Failure = NoSuchOutput String
             | OutputNotFound OutputId
             | MonitorNotFound MonitorId
             | OutputAlreadyDisabled Output
             | OutputNotEnabled Output
             | NoFreeMonitors
             | UnableToDeterminePreferredMode Output
             deriving Show

data SetupDifference = SetupDifference { setupDifferenceEnable :: [OutputId]
                                       , setupDifferenceDisable :: [OutputId]
                                       } deriving Show

compareDesiredSetup :: Setup -> DesiredSetup -> Either String SetupDifference
compareDesiredSetup setup desired = do
    requiredOutputs <- requiredOutputsE
    return $ SetupDifference { setupDifferenceEnable = requiredOutputs `intersect` disabledOutputs
                             , setupDifferenceDisable = (allOutputIds \\ requiredOutputs) `intersect` enabledOutputs
                             }
        where requiredOutputsE = sequence $ map requireOutput (desiredSetupOutputs desired)
              requireOutput name = outputId <$> maybe (notConnectedReason name) Right (findConnectedOutput name)
              notConnectedReason name = (Left $ "not applicable: output " ++ name ++ " not connected")
              findConnectedOutput name = mfilter isOutputConnected $ findOutput name setup
              allOutputs = M.elems $ setupOutputs setup
              allOutputIds = map outputId allOutputs
              (enabledOutputs, disabledOutputs) = bimap (map outputId) (map outputId) $ partition isOutputEnabled allOutputs

applyChanges :: (MonadState Setup m, MonadIO m) => Xlib.Display -> X.Window -> XRRScreenResources -> Setup -> Setup -> m ()
applyChanges display root res initialSetup setup = do
    if dimensionsGrew then (liftIO $ updateScreen display root dimAfter) else return ()
    forM_ (filter monitorChanged . M.elems $ setupMonitors setup) $ \monitor -> do
        0 <- liftIO $ updateMonitor display res monitor
        modify $ upsertMonitor monitor { monitorChanged = False }
    if dimensionsShrank then (liftIO $ updateScreen display root dimAfter) else return ()
    where
        dimBefore = calculateScreenDimensions initialSetup
        dimAfter = calculateScreenDimensions setup
        dimensionsGrew = dimAfter > dimBefore
        dimensionsShrank = dimAfter < dimBefore


doSwitcheroo :: IO ()
doSwitcheroo = do
    Right config <- loadConfig "example.json"

    display <- openDisplay ""
    root <- rootWindow display (defaultScreen display)
    Just res <- xrrGetScreenResourcesCurrent display root
    initialSetup <- fetchSetup display res

    putStrLn . show $ map (compareDesiredSetup initialSetup) (configDesiredSetups config)

    result <- (flip runStateT) initialSetup . runExceptT $ do
        a <- findOutputE "DVI-I-1"
        b <- findOutputE "DVI-D-1"
        c <- findOutputE "HDMI-1"
        _ <- b `rightOf` a
        _ <- c `sameAs` b

        get >>= applyChanges display root res initialSetup
    putStrLn . show $ result
