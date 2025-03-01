{-# LANGUAGE OverloadedRecordDot #-}

module WindowRules (setup) where

import Data.Ratio ((%))
import Workspaces hiding (setup)
import XMonad
import XMonad.Hooks.ManageHelpers
import XMonad.Prelude
import XMonad.StackSet
import XMonad.Util.Hacks


setup :: XConfig l -> XConfig l
setup c =
  c
    { manageHook = windowRules <+> c.manageHook
    , handleEventHook = hacks <> c.handleEventHook
    }


windowRules :: ManageHook
windowRules =
  composeOne
    [ appName =? "brave-browser" -?> shiftTo web
    , appName =? "whatsapp-for-linux" -?> shiftTo chat
    , className =? "thunderbird" -?> shiftTo mail
    , appName =? "pavucontrol" -?> smallCenteredFloat
    , className =? "Thunar" -?> doFloat
    , appName =? "slack" -?> shiftTo chat
    ]
  where
    smallCenteredFloat = doRectFloat (RationalRect (1 % 3) (1 % 3) (1 % 3) (1 % 3))


hacks :: Event -> X All
hacks = windowedFullscreenFixEventHook <> fixSteamFlicker
