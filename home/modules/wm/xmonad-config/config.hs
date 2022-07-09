import Data.Foldable (traverse_)

import XMonad
import qualified XMonad.StackSet as StackSet

import XMonad.Actions.DynamicProjects

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing

import Control.Monad (replicateM_)
import qualified DBus.Client as D
import qualified Polybar
import XMonad.Layout.Tabbed (simpleTabbed)
import XMonad.Util.NamedScratchpad
import qualified XMonad.Util.NamedWindows as NW
import XMonad.Util.Run


main :: IO ()
main =
  Polybar.initDbus >>= main'


main' :: D.Client -> IO ()
main' =
  xmonad
    . ewmhFullscreen
    . ewmh
    . docks
    . projects
    . urgencyHook
    . myConfig
  where
    projects = dynamicProjects myProjects
    urgencyHook = withUrgencyHook LibNotifyUrgencyHook


myConfig dbus =
  def
    { terminal = myTerminal
    , focusedBorderColor = "#2986cc"
    , borderWidth = 2
    , workspaces = myWorkspaces
    , layoutHook = myLayout
    , manageHook = myManageHook
    , logHook = Polybar.hook dbus
    }
  where
    myManageHook = namedScratchpadManageHook scratchpads


-- Taken from https://pbrisbin.com/posts/using-notify-osd-for-xmonad-notifications/
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)


instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name <- NW.getName w
    maybeIdx <- StackSet.findTag w <$> gets windowset
    traverse_ (\i -> safeSpawn "notify-send" [show name, "workspace " ++ i]) maybeIdx


myTerminal = "kitty"


----------------
-- WORKSPACES --
----------------

webWs = "web"
chatWs = "chat"
workWs = "work"
persoWs = "perso"
nixWs = "nix"


myWorkspaces :: [String]
myWorkspaces =
  [ webWs
  , chatWs
  , workWs
  , persoWs
  , nixWs
  ]


myProjects :: [Project]
myProjects =
  [ Project
      { projectName = webWs
      , projectDirectory = "~"
      , projectStartHook = Just $ do
          spawn "firefox"
      }
  , Project
      { projectName = chatWs
      , projectDirectory = "~"
      , projectStartHook = Just $ do
          spawn "slack"
          spawn "discord"
          spawn "chromium"
      }
  , Project
      { projectName = workWs
      , projectDirectory = "~/Work/stuart"
      , projectStartHook = Nothing
      }
  , Project
      { projectName = persoWs
      , projectDirectory = "~/Work/personal"
      , projectStartHook = Nothing
      }
  , Project
      { projectName = nixWs
      , projectDirectory = "~/Work/personal/nix-config"
      , projectStartHook = Just $ do
          spawn "code"
          replicateM_ 2 $ spawn myTerminal
      }
  ]


-------------
-- LAYOUTS --
-------------

myLayout =
  avoidStruts
    . smartSpacingWithEdge 5
    . smartBorders
    . webLayout
    . chatLayout
    . workLayout
    . persoLayout
    $ layout
  where
    layout = layoutHook def
    webLayout = onWorkspace webWs layout
    chatLayout = onWorkspace chatWs (simpleTabbed ||| Full)
    workLayout = onWorkspace workWs layout
    persoLayout = onWorkspace persoWs layout


-----------------
-- SCRATCHPADS --
-----------------

scratchpads = []
