-------------------------------------------------------------------------------
-- |
-- Module       : Tools
-- Copyright    : (C) Gaby Czegany 2012
-- License      : Public Domain
--
-- Maintainer   : Glabsounet@gmail.com
-- Stability    : unstable
-- Portability  : unportable
--
-- Some usefull tools used in ~/.xmonad/xmonad.hs
-------------------------------------------------------------------------------

module Tools
        ( 
        -- * Keybinds tools
          dmenuLaunch
        , dmenuLaunchPDF
        , restartXMonad
        ) where

import XMonad

-- | Launch dmenu with some custom option like:
--   -b     -> bottom bars
--   -fn    -> font, Inconsolata
dmenuLaunch :: MonadIO m => m ()
dmenuLaunch = spawn $ "exe=`ls /usr/bin | dmenu -b -fn Inconsolata-9:normal`"
                    ++ "&& eval \"exec $exe\""

-- | Launch dmenu. Print all pdf found recursively in stdin.
--   Then execute zathura with stdout text.
dmenuLaunchPDF :: MonadIO m => m ()
dmenuLaunchPDF = spawn $ "exe=`find . -name *.pdf | "
                    ++ "dmenu -b -p Zathura -fn Inconsolata-9:normal`"
                    ++ "&& eval \"exec zathura $exe\""

-- | A Clean XMonad restart 
restartXMonad :: MonadIO m => m ()
restartXMonad = spawn $ "xmonad --recompile ; xmonad --restart"
