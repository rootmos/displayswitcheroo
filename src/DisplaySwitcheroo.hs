module DisplaySwitcheroo
    ( doSwitcheroo
    ) where

import Graphics.X11 ( openDisplay
                    , rootWindow
                    , defaultScreen
                    )
import Graphics.X11.Xrandr
import qualified Graphics.X11.Types as X
import qualified Graphics.X11.Xlib.Types as Xlib
import Control.Monad ( liftM )
import Data.Maybe ( catMaybes )
import qualified Data.Map.Strict as M
import Data.Bifunctor ( second )

data Output = Output { outputName :: String
                     , outputModes :: [Mode]
                     }
                     deriving Show


data Monitor = Monitor { monitorX :: Int
                       , monitorY :: Int
                       , monitorWidth :: Int
                       , monitorHeight :: Int
                       , monitorMode :: Maybe Mode
                       }
                       deriving Show

data Mode = Mode { modeWidth :: Int
                 , modeHeight :: Int
                 }

instance Show Mode where
    show Mode { modeWidth = w, modeHeight = h } = show w ++ "x" ++ show h


modeMap :: XRRScreenResources -> M.Map X.RRMode Mode
modeMap XRRScreenResources { xrr_sr_modes = modes } =
    M.fromAscList $ map modeMaker modes
        where
            modeMaker XRRModeInfo { xrr_mi_id = mid, xrr_mi_width = width, xrr_mi_height = height} =
                (mid, Mode { modeWidth = (fromIntegral width), modeHeight = (fromIntegral height) })

connectedOutputs :: Xlib.Display -> XRRScreenResources -> IO (M.Map X.RROutput Output, M.Map X.RRCrtc Monitor)
connectedOutputs display res = do
    outputInfos <- liftM catMaybes $ mapM (\oid -> (fmap ((,) oid)) <$> xrrGetOutputInfo display res oid) (xrr_sr_outputs res)
    let outputs = M.fromAscList . map (second outputMaker) . filter (isConnected . snd) $ outputInfos

    crtcInfos <- liftM catMaybes $ mapM (\cid -> (fmap ((,) cid)) <$> xrrGetCrtcInfo display res cid) (xrr_sr_crtcs res)
    let monitors = M.fromAscList . map (second monitorMaker) $ crtcInfos

    return (outputs, monitors)
        where
            isConnected XRROutputInfo { xrr_oi_connection = 0 } = True
            isConnected _ = False

            outputMaker XRROutputInfo { xrr_oi_name = name, xrr_oi_modes = modes } =
                Output { outputName = name
                       , outputModes = catMaybes $ map (\m -> M.lookup m (modeMap res)) modes
                       }

            monitorMaker ci =
                Monitor { monitorX = (fromIntegral $ xrr_ci_x ci)
                        , monitorY = (fromIntegral $ xrr_ci_y ci)
                        , monitorWidth = (fromIntegral $ xrr_ci_width ci)
                        , monitorHeight = (fromIntegral $ xrr_ci_height ci)
                        , monitorMode = M.lookup (xrr_ci_mode ci) (modeMap res)
                        }


doSwitcheroo :: IO ()
doSwitcheroo = do
    display <- openDisplay ""
    root <- rootWindow display (defaultScreen display)
    Just res <- xrrGetScreenResourcesCurrent display root
    outputs <- connectedOutputs display res
    putStrLn . show $ outputs

