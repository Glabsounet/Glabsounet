-- Generic configuration for xmonad.
-- Gaby Czegany <Glabsounet@gmail>

import Default (glabsounetConfig)
import Tools

import System.IO (hPutStrLn)

import XMonad
import XMonad.Util.EZConfig (additionalKeysP)

main :: IO ()
main = do
    xmonad $ glabsounetConfig
        { modMask   = mod1Mask
        } `additionalKeysP` myKeys

myKeys :: [(String, X())]
myKeys = [ ("M-p"       , dmenuLaunch                   ) -- dmenu app launcher
         , ("M-S-p"     , dmenuLaunchPDF                ) -- dmenu pdf launcher
         , ("M-q"       , restartXMonad                 ) -- restart xmonad
         ]
         ++
         -- Music management key
         [ ("<XF86AudioPlay>"       , spawn "mocp -G")
         , ("<XF86AudioStop>"       , spawn "mocp --exit")
         , ("<XF86AudioLowerVolume>", spawn "amixer sset PCM 1-")
         , ("<XF86AudioRaiseVolume>", spawn "amixer sset PCM 1+")
         , ("<XF86AudioNext>"       , spawn "mocp --next")
         , ("<XF86AudioPrev>"       , spawn "mocp --previous")
         , ("M-a"                   , spawn "mocp -a \"`mocp -Q %file`\"")
         ]
