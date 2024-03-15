{-# LANGUAGE OverloadedRecordDot #-}

module Workspaces
  ( setup
  , web
  , chat
  , work
  , perso
  , nix
  , allWorkspaces
  , shiftTo
  )
where

import Data.List.NonEmpty (nonEmpty)
import XMonad
import XMonad.Actions.DynamicProjects (Project (..), dynamicProjects)
import XMonad.Actions.MouseResize
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.MultiToggle (mkToggle, single)
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Renamed (Rename (..), renamed)
import XMonad.Layout.Spacing (smartSpacing)
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowArranger (windowArrange)
import XMonad.Prelude
import XMonad.Util.SpawnOnce (spawnOnOnce)


setup c =
  setupProjects $
    c
      { workspaces = name <$> allWorkspaces
      , layoutHook = workspacesLayout c
      }
  where
    setupProjects = dynamicProjects $ toProject <$> allWorkspaces


data Workspace = Workspace
  { name :: WorkspaceId
  , workingDirectory :: String
  , runAtStartup :: [String]
  }


web :: Workspace
web =
  Workspace
    { name = "web"
    , workingDirectory = "~"
    , runAtStartup = ["chromium"]
    }


chat :: Workspace
chat =
  Workspace
    { name = "chat"
    , workingDirectory = "~"
    , runAtStartup = ["slack"]
    }


work :: Workspace
work =
  Workspace
    { name = "work"
    , workingDirectory = "~/Code/work"
    , runAtStartup = ["idea"]
    }


perso :: Workspace
perso =
  Workspace
    { name = "perso"
    , workingDirectory = "~/Code/perso"
    , runAtStartup = []
    }


-- TODO fix terminal setup
nix :: Workspace
nix =
  Workspace
    { name = "nix"
    , workingDirectory = "~/Code/perso/nix-config"
    , runAtStartup = ["code", "kitty"]
    }


allWorkspaces :: [Workspace]
allWorkspaces =
  [ web
  , chat
  , work
  , perso
  , nix
  ]


shiftTo :: Workspace -> ManageHook
shiftTo =
  doShift . name


toProject :: Workspace -> Project
toProject ws =
  Project
    { projectName = ws.name
    , projectDirectory = ws.workingDirectory
    , projectStartHook = startApps <$> nonEmpty ws.runAtStartup
    }
  where
    startApps = traverse_ $ spawnOnOnce ws.name


workspacesLayout c =
  avoidStruts
    . mouseResize
    . windowArrange
    . toggleFullScreen
    . dropSpacingLabel
    . smartSpacing 5
    . smartBorders
    . webLayout
    . chatLayout
    . workLayout
    . persoLayout
    $ layout
  where
    toggleFullScreen = mkToggle $ single NBFULL
    dropSpacingLabel = renamed [CutWordsLeft 1]
    layout = layoutHook def
    webLayout = forWorkspace web layout
    chatLayout = forWorkspace chat (tabs ||| Full)
    workLayout = forWorkspace work layout
    persoLayout = forWorkspace perso layout
    forWorkspace ws = onWorkspace (name ws)
    tabs = renamed [CutWordsRight 1] $ tabbed shrinkText tabTheme
    tabTheme =
      def
        { activeBorderColor = focusedBorderColor c
        , activeBorderWidth = borderWidth c
        , decoHeight = 20
        }
