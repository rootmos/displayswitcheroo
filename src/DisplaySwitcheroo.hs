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
import Data.Bifunctor ( bimap )


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


newtype MonitorId = MonitorId X.RRCrtc
    deriving ( Show, Eq, Ord )

data Monitor = Monitor { monitorId :: MonitorId
                       , monitorX :: Int
                       , monitorY :: Int
                       , monitorWidth :: Int
                       , monitorHeight :: Int
                       , monitorMode :: Maybe Mode
                       , monitorOutputs :: [OutputId]
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
                 , modeHz :: Int
                 }
                 deriving ( Eq )

instance Ord Mode where
    x `compare` y = (toTuple x) `compare` (toTuple y)
        where toTuple m = (modeWidth m, modeHeight m, modeHz m)

instance Show Mode where
    show Mode { modeWidth = w, modeHeight = h, modeHz = hz } =
        show w ++ "x" ++ show h ++ "(" ++ show hz ++ "Hz)"



modeMap :: XRRScreenResources -> M.Map ModeId Mode
modeMap XRRScreenResources { xrr_sr_modes = modes } =
    M.fromAscList $ map modeMaker modes
        where
            modeMaker mi =
                (ModeId (xrr_mi_id mi), Mode { modeId = ModeId (xrr_mi_id mi)
                                             , modeWidth = fromIntegral $ xrr_mi_width mi
                                             , modeHeight = fromIntegral $ xrr_mi_height mi
                                             , modeHz = refreshRate mi
                                             })

refreshRate :: XRRModeInfo -> Int
refreshRate mi = round $ (dotClock / (hTotal * vTotal) :: Double)
    where
        dotClock = fromIntegral $ xrr_mi_dotClock mi
        hTotal = fromIntegral $ xrr_mi_hTotal mi
        vTotal
          | doubleScanActive && not interlaceActive = 2 * (fromIntegral $ xrr_mi_vTotal mi)
          | not doubleScanActive && interlaceActive = (fromIntegral $ xrr_mi_vTotal mi) / 2
          | otherwise = fromIntegral $ xrr_mi_vTotal mi
        doubleScanActive = testBit (xrr_mi_modeFlags mi) 5
        interlaceActive = testBit (xrr_mi_modeFlags mi) 4

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
                        }

updateMonitor :: Xlib.Display -> XRRScreenResources -> Monitor -> IO X.Status
updateMonitor display res (monitor @ Monitor { monitorId = MonitorId cid, monitorMode = Nothing }) =
    xrrSetCrtcConfig display res cid currentTime 0 0 none X.xRR_Rotate_0 []

updateMonitor display res (monitor @ Monitor { monitorId = MonitorId cid
                                             , monitorMode = Just (Mode { modeId = ModeId mid })
                                             }) = do
    Just prevConfig <- xrrGetCrtcInfo display res cid
    let x = fromIntegral $ monitorX monitor
        y = fromIntegral $ monitorY monitor
        rot = xrr_ci_rotations prevConfig
        outputs = map (\(OutputId i) -> i) $ monitorOutputs monitor

    xrrSetCrtcConfig display res cid currentTime x y mid X.xRR_Rotate_0 outputs

updateScreen :: Xlib.Display -> X.Window -> (Int, Int, Int, Int) -> IO ()
updateScreen display root (width, height, widthInMillimeters, heightInMillimeters) =
    xrrSetScreenSize display root (fromIntegral width) (fromIntegral height) (fromIntegral widthInMillimeters) (fromIntegral heightInMillimeters)

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

rightOfMonitor :: Monad m => Output -> Monitor -> StateT Setup m (Maybe Monitor)
output `rightOfMonitor` existingMonitor = do
    Setup { setupMonitors = monitors } <- get
    let maybeMonitor = do
            disabledMonitor <- listToMaybe . filter (not . isMonitorEnabled) . catMaybes $
                map (flip M.lookup $ monitors) (outputMonitors output)
            mode <- preferredMode output
            return disabledMonitor { monitorMode = Just mode
                                   , monitorX = monitorX existingMonitor + monitorWidth existingMonitor
                                   , monitorY = monitorY existingMonitor
                                   , monitorWidth = modeWidth mode
                                   , monitorHeight = modeHeight mode
                                   , monitorOutputs = [outputId output]
                                   }
    case maybeMonitor of
      Just monitor -> do
          modify $ \setup -> setup { setupMonitors = M.insert (monitorId monitor) monitor (setupMonitors setup)
                                   , setupOutputs = M.insert (outputId output) (output { outputMonitor = Just (monitorId monitor) }) (setupOutputs setup)
                                   }
          return (Just monitor)
      Nothing -> return Nothing

rightOf :: Monad m => Output -> Output -> StateT Setup m (Maybe Monitor)
output `rightOf` existingOutput = get >>= \setup -> do
    case outputMonitor existingOutput >>= flip lookupMonitor setup of
      Just existingMonitor -> output `rightOfMonitor` existingMonitor
      Nothing -> return Nothing

findOutput :: String -> Setup -> Maybe Output
findOutput name = find ((== name) . outputName) . setupOutputs

lookupMonitor :: MonitorId -> Setup -> Maybe Monitor
lookupMonitor mid = M.lookup mid . setupMonitors

lookupOutput :: OutputId -> Setup -> Maybe Output
lookupOutput oid = M.lookup oid . setupOutputs


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

doSwitcheroo :: IO ()
doSwitcheroo = do
    Right config <- loadConfig "example.json"

    display <- openDisplay ""
    root <- rootWindow display (defaultScreen display)
    Just res <- xrrGetScreenResourcesCurrent display root
    initialSetup <- fetchSetup display res

    putStrLn . show $ map (compareDesiredSetup initialSetup) (configDesiredSetups config)
