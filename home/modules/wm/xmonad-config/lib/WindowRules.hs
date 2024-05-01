{-# LANGUAGE OverloadedRecordDot #-}

module WindowRules (setup) where

import Data.Ratio ((%))
import Workspaces hiding (setup)
import XMonad
import XMonad.Hooks.ManageHelpers
import XMonad.StackSet


setup :: XConfig l -> XConfig l
setup c =
  c
    { manageHook = windowRules <+> c.manageHook
    }


windowRules :: ManageHook
windowRules =
  composeOne
    [ appName =? "chromium-browser" -?> shiftTo web
    , appName =? "whatsapp-for-linux" -?> shiftTo chat
    , className =? "thunderbird" -?> shiftTo mail
    , appName =? "pavucontrol" -?> smallCenteredFloat
    , className =? "Thunar" -?> doFloat
    , appName =? "slack" -?> shiftTo chat
    ]
  where
    smallCenteredFloat = doRectFloat (RationalRect (1 % 3) (1 % 3) (1 % 3) (1 % 3))
