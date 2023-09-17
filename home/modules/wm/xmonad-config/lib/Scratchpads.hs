module Scratchpads (setup, run, weather) where

import qualified Terminal
import XMonad
import XMonad.Hooks.EwmhDesktops (addEwmhWorkspaceSort)
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare (filterOutWs)


setup :: XConfig a -> XConfig a
setup c =
  filterScratchpadsWorkspace $
    c {manageHook = manageScratchpads <+> manageHook c}
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
    (Terminal.runShow nsName ["-o font_size=12"] "curl 'https://wttr.in?m&F&Q&2&lang=fr'")
    (title =? nsName)
    defaultFloating
  where
    nsName = "weather"
