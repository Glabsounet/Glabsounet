import XMonad
import XMonad.ManageHook
import XMonad.Hooks.ManageDocks (manageDocks, avoidStruts)
import XMonad.Hooks.ManageHelpers (isDialog, isFullscreen, doFullFloat, doCenterFloat)
import XMonad.Actions.GridSelect (goToSelected, defaultGSConfig)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Layout.NoBorders (smartBorders)

import qualified XMonad.StackSet as W

myTerminal  = "urxvtc"
myFont      = "Inconsolata-9:normal"

runCmd :: MonadIO m => m ()
runCmd = spawn $ "exe=`dmenu_run -b -fn " ++ myFont ++ "` && eval \"exec $exe\""

runPdf :: MonadIO m => m ()
runPdf = spawn $ "exe=`find . -name *.pdf |"
               ++ "dmenu -b -p Zathura -fn " ++ myFont ++ "`"
               ++ "&& eval \"exec zathura $exe\""

myKeys :: [(String, X())]
myKeys =
    [ ("M-q", myRestart)
    , ("M-w", goToSelected defaultGSConfig)
    , ("M-p", runCmd)
    , ("M-S-p", runPdf)
    , ("<XF86AudioMute>", spawn "amixer set Master toggle+")
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 10%-")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 10%+")
    ] where myRestart = spawn $ "xmonad --recompile ; xmonad --restart"

myManageHook :: ManageHook
myManageHook = composeAll $ concat
    [ [ manageDocks]
    , [ classRole =? "browser" --> doShift "1"]
    , [ className =? "Pidgin" --> doShift "9"]
    , [ className =? "Xmessage" --> doCenterFloat]
    , [ className =? "Plugin-container" --> doCenterFloat]
    , [ isDialog --> doCenterFloat]
    ] where classRole = stringProperty "WM_WINDOW_ROLE"

myLayoutHook = avoidStruts . smartBorders $ Tall 1 (3 / 100) (1 / 2) ||| Full

main :: IO ()
main = do
    xmonad $ defaultConfig
        { terminal              = myTerminal
        , modMask               = mod1Mask
        , normalBorderColor     = "#000000"
        , focusedBorderColor    = "#535d6c"
        , workspaces            = map show [1..9 :: Int]
        , manageHook            = myManageHook
        , layoutHook            = myLayoutHook
        } `additionalKeysP` myKeys

