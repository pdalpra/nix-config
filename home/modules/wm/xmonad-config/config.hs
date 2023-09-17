module Main (main) where

import qualified Bindings
import qualified KeyboardLayout
import qualified Notifications
import qualified Polybar
import qualified Scratchpads
import qualified Terminal
import qualified WindowRules
import qualified Workspaces
import XMonad
import XMonad.Actions.SpawnOn (manageSpawn)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.FadeInactive (fadeInactiveCurrentWSLogHook)
import XMonad.Hooks.ManageDocks (docks)


main :: IO ()
main = do
  polybar <- Polybar.setup
  xmonad
    . KeyboardLayout.setup
    . Bindings.setup
    . Terminal.setup
    . Scratchpads.setup
    . WindowRules.setup
    . Workspaces.setup
    . Notifications.setup
    . (ewmhFullscreen . ewmh)
    . polybar
    . docks
    $ myConfig


myConfig =
  def
    { focusedBorderColor = "#2986cc"
    , borderWidth = 2
    , manageHook = manageSpawn
    , logHook = fadeInactiveCurrentWSLogHook 0.7
    }
