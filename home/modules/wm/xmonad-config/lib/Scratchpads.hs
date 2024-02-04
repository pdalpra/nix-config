{-# LANGUAGE OverloadedRecordDot #-}

module Scratchpads (setup, run, weather) where

import Data.Ratio ((%))
import qualified Terminal
import XMonad
import XMonad.Hooks.EwmhDesktops (addEwmhWorkspaceSort)
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare (filterOutWs)


setup :: XConfig a -> XConfig a
setup c =
  filterScratchpadsWorkspace $
    c {manageHook = manageScratchpads <+> c.manageHook}
  where
    filterScratchpadsWorkspace = (addEwmhWorkspaceSort . pure . filterOutWs) [scratchpadWorkspaceTag]
    manageScratchpads = namedScratchpadManageHook scratchpads


scratchpads :: NamedScratchpads
scratchpads =
  [ weather
  ]


run :: NamedScratchpad -> X ()
run = namedScratchpadAction scratchpads . name


weather :: NamedScratchpad
weather =
  NS
    nsName
    (Terminal.runShow nsName ["-o font.size=15"] command)
    (title =? nsName)
    (customFloating $ W.RationalRect (1 % 4) (1 % 3) (2 % 4) (1 % 3))
  where
    nsName = "weather"
    command = "curl -s \"https://wttr.in?m&F&Q&2&lang=fr\" | less"
