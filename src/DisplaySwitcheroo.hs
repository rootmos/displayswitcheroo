module DisplaySwitcheroo
    ( doSwitcheroo
    ) where

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
import Data.Maybe ( catMaybes )
import qualified Data.Map.Strict as M
import Data.Bits ( testBit )


newtype OutputId = OutputId X.RROutput
    deriving ( Show, Eq )

data Output = Output { outputId :: OutputId
                     , outputName :: String
                     , outputModes :: [Mode]
                     , outputMonitor :: Maybe MonitorId
                     , outputMonitors :: [MonitorId]
                     }
                     deriving Show

isConnected :: Output -> Bool
isConnected Output { outputModes = [] } = False
isConnected _ = True

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

newtype ModeId = ModeId X.RRMode
    deriving ( Show, Eq, Ord)

data Mode = Mode { modeId :: ModeId
                 , modeWidth :: Int
                 , modeHeight :: Int
                 , modeHz :: Int
                 }

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

monitorsAndOutputs :: Xlib.Display -> XRRScreenResources -> IO (M.Map OutputId Output, M.Map MonitorId Monitor)
monitorsAndOutputs display res = do
    rawOutputs <- rawOutputsM
    let outputs = M.mapWithKey outputMaker rawOutputs

    rawMonitors <- rawMonitorsM
    let monitors = M.mapWithKey monitorMaker rawMonitors

    return (outputs, monitors)
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

    xrrSetCrtcConfig display res cid currentTime x y mid rot outputs

doSwitcheroo :: IO ()
doSwitcheroo = do
    display <- openDisplay ""
    root <- rootWindow display (defaultScreen display)
    Just res <- xrrGetScreenResourcesCurrent display root
    (outputs, monitors) <- monitorsAndOutputs display res
    forM_ outputs $ putStrLn . show
    forM_ monitors $ putStrLn . show

    let Just m = M.lookup (MonitorId 97) monitors
        newSettings = m { monitorMode = Nothing }
    updateMonitor display res newSettings >>= putStrLn . show

