-- Generic configuration for xmonad.
-- Gaby Czegany <Glabsounet@gmail>

import System.IO (hPutStrLn)

import XMonad
import XMonad.ManageHook
import XMonad.Hooks.ManageDocks (manageDocks, avoidStruts)
import XMonad.Hooks.ManageHelpers (isDialog, isFullscreen, doFullFloat, doCenterFloat)
import XMonad.Actions.GridSelect (goToSelected, defaultGSConfig)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Scratchpad (scratchpadManageHook, scratchpadSpawnActionTerminal)
import XMonad.Util.NamedScratchpad
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.LayoutCombinators hiding ((|||))
import XMonad.Layout.Tabbed

import qualified XMonad.StackSet as W

main :: IO ()
main = do xmonad $ defaultConfig
    { modMask               = mod1Mask
    , key                   = myKeys
    , manageHook            = newManageHook
    , terminal              = "urxvt"
    , newNormalBorderColor  = "#222222"
    , newFocusedBorderColor = "#777777"
    , Workspaces            = ["home", "web", "misc"] ++ map show [4..9 :: Int] ++ ["NSP"]
    }

myKeys :: [(String, X())]
myKeys = 
    [ ("M-p",   dmenuLaunch)
    , ("M-w",   goToSelected defaultGSConfig)
    , ("M-q",   restartXMonad)
    ]
    ++
    -- Sound key
    [ ("<XF86AudioLowerVolume>", spawn "amixer sset PCM 1-")
    , ("<XF86AudioRaiseVolume>", spawn "amixer sset PCM 1+")
    ]
    ++
    -- Scratchpad key
    [ ("M-w",   goToSelected defaultGSConfig)
    , ("M-s",   scratchpadSpawnActionTerminal newTerminal)
    , ("M-S-s", namedScratchpadAction newScratchpads "main")

restartXMonad = spawn $ "xmonad --recompile ; xmonad --restart"
dmenuLaunch = spawn $ "exe=`ls /usr/bin | dmenu -b -fn Inconsolata-9:normal`"
                    ++ "&& eval \"exec $exe\""

myScratchpads :: [NamedScratchpad]
myScratchpads = 
    [ NS "main" (newTerminal ++ "") (title =? "main")
        (customFloating $ W.RationalRect 0.0 0.7 1 0.3)
    ]

myManageHook :: ManageHook
myManageHook = composeAll $ concat 
    [ [ manageDocks                                         ]
    , [ classRole =? "browser"      --> doShift "web"       ]
    , [ className =? "Xmessage"     --> doCenterFloat       ]
    , [ className =? flashPlayer    --> doCenterFloat       ]
    , [ isDialog                    --> doCenterFloat       ]
    , [ scratchpadManageHook $ W.RationalRect 0 0.7 1 0.3   ]
    , [ namedScratchpadManageHook newScratchpads            ]
    ] where classRole = stringProperty "WM_WINDOW_ROLE"
            flashPlayer = "Plugin-container"

