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

data Display = Display { displayName :: String
                       , displayModes :: [Mode]
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

connectedDisplays :: Xlib.Display -> XRRScreenResources -> IO [Display]
connectedDisplays display res = do
    outputInfos <- liftM catMaybes $ mapM (xrrGetOutputInfo display res) (xrr_sr_outputs res)
    return $ map displayMaker . filter isConnected $ outputInfos
        where
            isConnected XRROutputInfo { xrr_oi_connection = 0 } = True
            isConnected _ = False

            displayMaker XRROutputInfo { xrr_oi_name = name, xrr_oi_modes = modes } =
                Display { displayName = name
                        , displayModes = catMaybes $ map (\m -> M.lookup m (modeMap res)) modes
                        }

doSwitcheroo :: IO ()
doSwitcheroo = do
    display <- openDisplay ""
    root <- rootWindow display (defaultScreen display)
    Just res <- xrrGetScreenResourcesCurrent display root
    displays <- connectedDisplays display res
    putStrLn . show $ displays

