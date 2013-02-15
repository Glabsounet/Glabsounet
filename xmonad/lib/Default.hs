module Default
        ( 
        -- * Simple configuration
          newTerminal
        , newModMask
        , newWorkspaces
        , newNormalBorderColor
        , newFocusedBorderColor

        -- * Advanced configuration
        , newScratchpads
        , newKey

        -- * Hook, Layout configuration
        , newManageHook

        -- * Main configuration
        , glabsounetConfig
        ) where

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

newTerminal :: [Char]
newTerminal = "urxvtc"

newModMask :: KeyMask
newModMask = mod1Mask

newWorkspaces :: [WorkspaceId]
newWorkspaces = ["home", "web", "misc"] ++ map show [4..9 :: Int] ++ ["NSP"]

newNormalBorderColor :: [Char]
newNormalBorderColor  = "#222222"

newFocusedBorderColor :: [Char]
newFocusedBorderColor = "#777777"

newScratchpads :: [NamedScratchpad]
newScratchpads = 
    [ NS "htop" (newTerminal ++ " -e htop") (title =? "htop")
        (customFloating $ W.RationalRect 0.6 0 0.4 0.8)
    , NS "rtorrent" (newTerminal ++ " -e rtorrent") (title =? "rtorrent")
        (customFloating $ W.RationalRect 0 0 0.6 0.8)
    , NS "alsamixer" (newTerminal ++ " -e alsamixer") (title =? "alsamixer") 
        doCenterFloat
    ]

newKey :: [(String, X())] 
newKey =  
    [ ("M-b",   spawn "firefox || $BROWSER"                     )
    , ("M-w",   goToSelected defaultGSConfig                    )
    ]
    ++
    -- Scratchpad key
    [ ("M-s",   scratchpadSpawnActionTerminal newTerminal       )
    , ("M-S-s", namedScratchpadAction newScratchpads "alsamixer")
    , ("M-d",   namedScratchpadAction newScratchpads "rtorrent" )
    , ("M-S-l", namedScratchpadAction newScratchpads "htop"     )
    ]

newManageHook :: ManageHook
newManageHook = composeAll $ concat 
    [ [ manageDocks                                         ]
    , [ classRole =? "browser"      --> doShift "web"       ]
    , [ className =? "Xmessage"     --> doCenterFloat       ]
    , [ className =? flashPlayer    --> doCenterFloat       ]
    , [ isDialog                    --> doCenterFloat       ]
    , [ scratchpadManageHook $ W.RationalRect 0 0.7 1 0.3   ]
    , [ namedScratchpadManageHook newScratchpads            ]
    , [ className =? "Gimp"  --> doShift "misc"             ]
    ] where classRole = stringProperty "WM_WINDOW_ROLE"
            flashPlayer = "Plugin-container"

newLayoutHook = avoidStruts . smartBorders $
    Tall 1 (3 / 100) (1 / 2) ||| gimpLayout ||| Full
  where
    tabbedLayout = tabbedBottomAlways shrinkText defaultTheme
    gimpLayout = tabbedLayout ****||* Full

glabsounetConfig  = defaultConfig
    { terminal              = newTerminal
    , modMask               = newModMask
    , workspaces            = newWorkspaces
    , normalBorderColor     = newNormalBorderColor
    , focusedBorderColor    = newFocusedBorderColor
    , manageHook            = newManageHook
    , layoutHook            = newLayoutHook
    } `additionalKeysP` newKey
