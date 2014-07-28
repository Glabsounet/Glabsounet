import XMonad
import XMonad.Actions.Submap (submap)
import XMonad.Hooks.ManageDocks (manageDocks, avoidStruts)
import XMonad.Hooks.ManageHelpers (isDialog, isFullscreen, doFullFloat, doCenterFloat)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Minimize
import XMonad.Layout.BoringWindows
import XMonad.ManageHook
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Scratchpad

import qualified XMonad.StackSet as W

myFont = "Inconsolata-9:normal"

runCmd :: MonadIO m => m ()
runCmd = spawn $ "exe=`dmenu_run -b -fn " ++ myFont ++ "` && eval \"exec '$exe'\""

runPdf :: MonadIO m => m ()
runPdf = spawn $ "exe=`find ~ -name *.pdf |"
               ++ "dmenu -b -p Zathura -fn " ++ myFont ++ "`"
               ++ "&& eval \"exec zathura '$exe'\""

myKeys :: [(String, X())]
myKeys =
    [ ("M-q", return ()) -- do nothing
    , ("M-S-q", myRestart)

    , ("M-w", do
        markBoring
        withFocused minimizeWindow)

    , ("M-S-w", do
        sendMessage RestoreNextMinimizedWin
        clearBoring)

    , ("M-k", focusUp)
    , ("M-j", focusDown)
    , ("M-m", focusMaster)

    , ("M-p", runCmd)
    , ("M-S-p", runPdf)

    , ("M-s", scratchpadSpawnActionTerminal "urxvtc")

    , ("<XF86AudioMute>", spawn "amixer set Master toggle+")
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 10%-")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 10%+")
    ] where myRestart = spawn $ "xmonad --recompile ; xmonad --restart"

myManageHook :: ManageHook
myManageHook = composeAll $ concat
    [ [ manageDocks]
    , [ classRole =? "browser"          --> doShift "1"]
    , [ classRole =? "gimp-toolbox"     --> doFloat]
    , [ classRole =? "gimp-dock"        --> doFloat]
    , [ classRole =? "vlc-video"        --> doCenterFloat]
    , [ className =? "Xmessage"         --> doCenterFloat]
    , [ className =? "Plugin-container" --> doCenterFloat]
    , [ classRole =? "TamperData"       --> doCenterFloat]
    , [ classRole =? "Preferences"      --> doCenterFloat]
    , [ isDialog                        --> doCenterFloat]
    , [ scratchpadManageHook (W.RationalRect 0.4 0.1 0.6 0.8)]
    ] where classRole = stringProperty "WM_WINDOW_ROLE"

myLayoutHook = boringWindows . minimize . avoidStruts . smartBorders $ Tall 1 (3 / 100) (1 / 2) ||| Full

main :: IO ()
main = do
    xmonad $ defaultConfig
        { terminal              = "urxvtc"
        , normalBorderColor     = "#000000"
        , focusedBorderColor    = "#535d6c"
        , modMask               = mod4Mask
        , workspaces            = map show [1..9 :: Int]
        , manageHook            = myManageHook
        , layoutHook            = myLayoutHook
        } `additionalKeysP` myKeys

