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
    [ appName =? "pavucontrol" -?> smallCenteredFloat
    , appName =? "Thunar" -?> doFloat
    , appName =? "slack" -?> shiftTo chat
    ]
  where
    smallCenteredFloat = doRectFloat (RationalRect (1 % 3) (1 % 3) (1 % 3) (1 % 3))
