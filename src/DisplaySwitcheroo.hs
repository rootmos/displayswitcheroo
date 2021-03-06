{-# LANGUAGE FlexibleContexts, ImpredicativeTypes, ExistentialQuantification #-}
module DisplaySwitcheroo
    ( fetchSetup
    , topLeft
    , rightOf
    , rightOfMonitor
    , sameAs
    , sameAsMonitor
    , findOutputE
    , lookupOutputE
    , applyChanges
    , disable
    , changes
    , compareDesiredSetup
    , setupDifferenceEnable
    , setupDifferenceDisable
    , setupDifferenceDesiredSetup
    , runDisplaySwitcheroo
    , isOutputEnabled
    , isOutputConnected
    , isMonitorEnabled
    , Setup(..)
    , Output(..)
    , Monitor(..)
    , Mode(..)
    , lookupMonitor
    , dpiX
    , dpiY
    ) where

import DisplaySwitcheroo.Config
import DisplaySwitcheroo.Logging

import Graphics.X11.Xlib.Extras ( currentTime, none )
import Graphics.X11.Xrandr
import qualified Graphics.X11.Types as X
import qualified Graphics.X11.Xlib.Types as Xlib
import Data.Maybe ( catMaybes
                  , listToMaybe
                  )
import Data.List ( unzip4
                 , find
                 , intersect
                 , (\\)
                 , sortBy
                 )
import qualified Data.Map.Strict as M
import Data.Bits ( testBit )
import Control.Monad.State.Strict
import Control.Monad.Except
import Text.Printf ( printf )
import Safe

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

preferredMode :: Maybe Resolution -> Output -> Maybe Mode
preferredMode Nothing o = maximumMay $ outputModes o
preferredMode (Just r) o = maximumMay $ rf . wf . hf $ outputModes o
  where wf = case resolutionWidth r of
               Nothing -> id
               Just w -> filter (\m -> modeWidth m <= w)
        hf = case resolutionHeight r of
               Nothing -> id
               Just h -> filter (\m -> modeHeight m <= h)
        rf = case resolutionRefreshRate r of
               Nothing -> id
               Just f -> filter (\m -> modeHz m <= f)

preferredModeE :: MonadError Failure m => Maybe Resolution -> Output -> m Mode
preferredModeE r output = maybe (throwError $ UnableToDeterminePreferredMode output) return $ preferredMode r output

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

data Field a = forall b. Eq b => Field (a -> b)

instance Eq Monitor where
    m == n = and $ map (\(Field f) -> f m == f n) fields
        where
            fields = [ Field monitorId
                     , Field monitorX
                     , Field monitorY
                     , Field monitorWidth
                     , Field monitorHeight
                     , Field monitorMode
                     , Field monitorOutputs
                     ]

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
        where toTuple m = (modeWidth m, modeHeight m, not . modeInterlaced $ m, not . modeDoubleScan $ m, modeHz m)

instance Show Mode where
    show mode = printf "%dx%d%s(%.2fHz)" (modeWidth mode) (modeHeight mode) flags (modeHz mode)
        where flags = if (modeInterlaced mode) then "i" else "" ++ if (modeDoubleScan mode) then "d" else ""

modeMap :: XRRScreenResources -> M.Map ModeId Mode
modeMap XRRScreenResources { xrr_sr_modes = modes } =
    M.fromList $ map modeMaker modes
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


data Failure = NoSuchOutput String
             | OutputNotFound OutputId
             | MonitorNotFound MonitorId
             | OutputAlreadyDisabled Output
             | OutputNotEnabled Output
             | NoFreeMonitors
             | UnableToDeterminePreferredMode Output
             deriving Show

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
            rawOutputsM = liftM makeMapFromMaybes $ forM (xrr_sr_outputs res) (\oid -> do
                maybeOi <- xrrGetOutputInfo display res oid
                return $ ((,) (OutputId oid)) <$> maybeOi)

            rawMonitorsM = liftM makeMapFromMaybes $ forM (xrr_sr_crtcs res) (\cid -> do
                maybeCi <- xrrGetCrtcInfo display res cid
                return $ ((,) (MonitorId cid)) <$> maybeCi)

            makeMapFromMaybes :: Ord k => [Maybe (k, v)] -> M.Map k v
            makeMapFromMaybes = M.fromAscList . sortBy (\x y -> fst x `compare` fst y) . catMaybes

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

data SetupDifference = SetupDifference { setupDifferenceDesiredSetup :: DesiredSetup
                                       , setupDifferenceEnable :: [OutputId]
                                       , setupDifferenceDisable :: [OutputId]
                                       } deriving Show


updateMonitor :: MonadIO m => Xlib.Display -> XRRScreenResources -> Monitor -> m X.Status
updateMonitor display res (monitor @ Monitor { monitorId = MonitorId cid, monitorMode = Nothing }) = do
    liftIO . debugL $ "Disabling monitor: " ++ show monitor
    liftIO $ xrrSetCrtcConfig display res cid currentTime 0 0 none X.xRR_Rotate_0 []

updateMonitor display res (monitor @ Monitor { monitorId = MonitorId cid
                                             , monitorMode = Just (Mode { modeId = ModeId mid })
                                             }) = do
    liftIO . debugL $ "Updating monitor: " ++ show monitor
    let x = fromIntegral $ monitorX monitor
        y = fromIntegral $ monitorY monitor
        outputs = map (\(OutputId i) -> i) $ monitorOutputs monitor

    liftIO $ xrrSetCrtcConfig display res cid currentTime x y mid X.xRR_Rotate_0 outputs

updateScreen :: MonadIO m => Xlib.Display -> X.Window -> (Int, Int, Int, Int) -> m ()
updateScreen display root dim@(width, height, widthInMillimeters, heightInMillimeters) = do
    liftIO . debugL $ "Updating screen dimensions: " ++ show dim
    liftIO $ xrrSetScreenSize display root (fromIntegral width) (fromIntegral height) (fromIntegral widthInMillimeters) (fromIntegral heightInMillimeters)

calculateScreenDimensions :: Setup -> Maybe (Int, Int, Int, Int)
calculateScreenDimensions Setup { setupOutputs = outputs, setupMonitors = monitors } = do
    maxX <- maximumMay maxXs
    maxY <- maximumMay maxYs
    denX <- maximumMay denXs
    denY <- maximumMay denYs
    let maxXmm = round $ (fromIntegral maxX) / denX
        maxYmm = round $ (fromIntegral maxY) / denY
    return (maxX, maxY, maxXmm, maxYmm)
    where
        (maxXs, maxYs, denXs, denYs) = unzip4 . catMaybes $ map considerOutput (M.elems outputs)
        considerOutput :: Output -> Maybe (Int, Int, Float, Float)
        considerOutput output = do
            mid <- outputMonitor output
            monitor <- M.lookup mid monitors
            let mX = monitorX monitor + monitorWidth monitor
                mY = monitorY monitor + monitorHeight monitor
                dX = densityX monitor output
                dY = densityY monitor output
            return (mX, mY, dX, dY)


densityX :: (Num t, Fractional t) => Monitor -> Output -> t
densityX monitor output = (fromIntegral $ monitorWidth monitor) / (fromIntegral $ outputWidthInMillimeters output)

dpiX :: (Num t, Fractional t) => Monitor -> Output -> t
dpiX monitor output = (densityX monitor output) * 25.4

densityY :: (Num t, Fractional t) => Monitor -> Output -> t
densityY monitor output = (fromIntegral $ monitorHeight monitor) / (fromIntegral $ outputHeightInMillimeters output)

dpiY :: (Num t, Fractional t) => Monitor -> Output -> t
dpiY monitor output = (densityY monitor output) * 25.4

compareAndTagAsChanged :: Monitor -> Monitor -> Monitor
compareAndTagAsChanged old new = new { monitorChanged = not $ new == old }

rightOfMonitor :: (MonadError Failure m, MonadState Setup m)
               => Maybe Resolution -> Output -> Monitor -> m Monitor
rightOfMonitor r output existingMonitor = do
    mode <- preferredModeE r output
    monitor <- case output of
      Output { outputMonitor = Nothing } -> freeMonitor output
      Output { outputMonitor = Just mid } -> lookupMonitorE mid

    let newMonitor = monitor { monitorMode = Just mode
                             , monitorX = monitorX existingMonitor + monitorWidth existingMonitor
                             , monitorY = monitorY existingMonitor
                             , monitorWidth = modeWidth mode
                             , monitorHeight = modeHeight mode
                             , monitorOutputs = [outputId output]
                             }
        modifiedMonitor = compareAndTagAsChanged monitor newMonitor
        modifiedOutput = output { outputMonitor = Just (monitorId modifiedMonitor) }
    modify $ upsertMonitor modifiedMonitor . upsertOutput modifiedOutput
    return modifiedMonitor

freeMonitor :: (MonadError Failure m, MonadState Setup m) => Output -> m Monitor
freeMonitor output = get >>= \Setup { setupMonitors = monitors } ->
    maybe (throwError NoFreeMonitors) return $
        listToMaybe . filter (not . isMonitorEnabled) . catMaybes $
            map (flip M.lookup $ monitors) (outputMonitors output)

rightOf :: (MonadError Failure m, MonadState Setup m) => Maybe Resolution -> Output -> Output -> m Monitor
rightOf _ _ (existingOutput @ Output { outputMonitor = Nothing }) = throwError $ OutputNotEnabled existingOutput
rightOf r output (Output { outputMonitor = Just mid }) = do
    monitor <- lookupMonitorE mid
    rightOfMonitor r output monitor

topLeft :: (MonadError Failure m, MonadState Setup m) => Maybe Resolution -> Output -> m Monitor
topLeft r output = do
    mode <- preferredModeE r output
    monitor <- case output of
      Output { outputMonitor = Nothing } -> freeMonitor output
      Output { outputMonitor = Just mid } -> lookupMonitorE mid

    let newMonitor = monitor { monitorMode = Just mode
                             , monitorX = 0
                             , monitorY = 0
                             , monitorWidth = modeWidth mode
                             , monitorHeight = modeHeight mode
                             , monitorOutputs = [outputId output]
                             }
        modifiedMonitor = compareAndTagAsChanged monitor newMonitor
        modifiedOutput = output { outputMonitor = Just (monitorId modifiedMonitor) }

    modify $ upsertMonitor modifiedMonitor . upsertOutput modifiedOutput
    return modifiedMonitor

sameAsMonitor :: (MonadError Failure m, MonadState Setup m) => Output -> Monitor -> m Monitor
sameAsMonitor output existingMonitor = do
    monitor <- case output of
      Output { outputMonitor = Nothing } -> freeMonitor output
      Output { outputMonitor = Just mid } -> lookupMonitorE mid

    mode <- maybe (throwError $ UnableToDeterminePreferredMode output) return $ monitorMode existingMonitor

    let newMonitor = monitor { monitorMode = Just mode
                             , monitorX = monitorX existingMonitor
                             , monitorY = monitorY existingMonitor
                             , monitorWidth = modeWidth mode
                             , monitorHeight = modeHeight mode
                             , monitorOutputs = [outputId output]
                             }
        modifiedMonitor = compareAndTagAsChanged monitor newMonitor
        modifiedOutput = output { outputMonitor = Just (monitorId modifiedMonitor) }

    modify $ upsertMonitor modifiedMonitor . upsertOutput modifiedOutput
    return modifiedMonitor

sameAs :: (MonadError Failure m, MonadState Setup m) => Output -> Output -> m Monitor
_ `sameAs` (existingOutput @ Output { outputMonitor = Nothing }) = throwError $ OutputNotEnabled existingOutput
output `sameAs` (Output { outputMonitor = Just mid }) = do
    monitor <- lookupMonitorE mid
    output `sameAsMonitor` monitor

disable :: (MonadError Failure m, MonadState Setup m) => Output -> m Monitor
disable o @ Output { outputMonitor = Nothing } = throwError $ OutputAlreadyDisabled o
disable output @ Output { outputMonitor = Just mid } = do
    monitor <- lookupMonitorE mid
    let newMonitor = monitor { monitorMode = Nothing }
        modifiedMonitor = compareAndTagAsChanged monitor newMonitor
        modifiedOutput = output { outputMonitor = Nothing }
    modify $ upsertMonitor modifiedMonitor . upsertOutput modifiedOutput
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
lookupMonitor i = M.lookup i . setupMonitors

lookupMonitorE :: (MonadError Failure m, MonadState Setup m) => MonitorId -> m Monitor
lookupMonitorE i = (gets $ lookupMonitor i) >>= maybe (throwError $ MonitorNotFound i) return

lookupOutput :: OutputId -> Setup -> Maybe Output
lookupOutput i = M.lookup i . setupOutputs

lookupOutputE :: (MonadError Failure m, MonadState Setup m) => OutputId -> m Output
lookupOutputE i = (gets $ lookupOutput i) >>= maybe (throwError $ OutputNotFound i) return

compareDesiredSetup :: Setup -> DesiredSetup -> Either String SetupDifference
compareDesiredSetup setup desired = do
    requiredOutputs <- requiredOutputsE
    return $ SetupDifference { setupDifferenceDesiredSetup = desired
                             , setupDifferenceEnable = requiredOutputs
                             , setupDifferenceDisable = (allOutputIds \\ requiredOutputs) `intersect` enabledOutputs
                             }
        where requiredOutputsE = sequence $ map requireOutput (desiredSetupOutputs desired)
              requireOutput name = outputId <$> maybe (notConnectedReason name) Right (findConnectedOutput name)
              notConnectedReason name = (Left $ "not applicable: output " ++ name ++ " not connected")
              findConnectedOutput name = mfilter isOutputConnected $ findOutput name setup
              allOutputs = M.elems $ setupOutputs setup
              allOutputIds = map outputId allOutputs
              enabledOutputs = map outputId $ filter isOutputEnabled allOutputs

applyChanges :: (MonadState Setup m, MonadIO m) => Xlib.Display -> X.Window -> XRRScreenResources -> Setup -> Setup -> m ()
applyChanges display root res initialSetup setup = do
    case (dimensionsGrew, maybeDimAfter) of
      (True, Just dimAfter) -> liftIO $ updateScreen display root dimAfter
      (_, Nothing) -> liftIO . warningL $ "Unable to determine needed screen dimensions, can't update screen. Setup: " ++ show setup
      (_, _) -> return ()

    forM_ negativeChanges doMonitorUpdate

    case (dimensionsShrank, maybeDimAfter) of
      (True, Just dimAfter) -> liftIO $ updateScreen display root dimAfter
      _ -> return ()

    forM_ positiveChanges doMonitorUpdate
    where
        doMonitorUpdate monitor = do
            status <- liftIO $ updateMonitor display res monitor
            if status == 0
               then modify $ upsertMonitor monitor { monitorChanged = False }
               else liftIO . warningL $ printf "Unable to update monitor. Status: %s Monitor: %s" (show status) (show monitor)
        positiveChanges = filter isMonitorEnabled $ changes setup
        negativeChanges = filter (not . isMonitorEnabled) $ changes setup
        maybeDimBefore = calculateScreenDimensions initialSetup
        maybeDimAfter = calculateScreenDimensions setup
        dimensionsGrew = case (maybeDimBefore, maybeDimAfter) of
                           (Nothing, Just _) -> True
                           (Just _ , Nothing) -> False
                           (Nothing , Nothing) -> False
                           (Just b, Just a) -> a >= b
        dimensionsShrank = not dimensionsGrew

changes :: Setup -> [Monitor]
changes = filter monitorChanged . M.elems . setupMonitors

runDisplaySwitcheroo :: MonadIO m => Setup -> ExceptT Failure (StateT Setup m) a -> m (Either Failure a, Setup)
runDisplaySwitcheroo initialSetup = (flip runStateT) initialSetup . runExceptT
