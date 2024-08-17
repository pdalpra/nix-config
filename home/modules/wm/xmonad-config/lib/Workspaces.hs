{-# LANGUAGE OverloadedRecordDot #-}

module Workspaces
  ( setup
  , web
  , mail
  , chat
  , nix
  , work
  , perso
  , allWorkspaces
  , shiftTo
  )
where

import Data.List.NonEmpty (nonEmpty)
import Terminal (term)
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
    , runAtStartup = []
    }


mail :: Workspace
mail =
  Workspace
    { name = "mail"
    , workingDirectory = "~"
    , runAtStartup = []
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
    , runAtStartup = ["code", term]
    }


nix :: Workspace
nix =
  Workspace
    { name = "nix"
    , workingDirectory = "~/Code/nix-config"
    , runAtStartup = ["code", term]
    }


allWorkspaces :: [Workspace]
allWorkspaces =
  [ web
  , mail
  , chat
  , nix
  , work
  , perso
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
    . mailLayout
    . chatLayout
    . nixLayout
    . workLayout
    . persoLayout
    $ layout
  where
    toggleFullScreen = mkToggle $ single NBFULL
    dropSpacingLabel = renamed [CutWordsLeft 1]
    layout = layoutHook def
    codeLayout = Tall 1 (1 / 2) (2 / 3) ||| Full
    webLayout = forWorkspace web (tabs ||| Full)
    mailLayout = forWorkspace mail Full
    chatLayout = forWorkspace chat (tabs ||| Full)
    nixLayout = forWorkspace nix codeLayout
    workLayout = forWorkspace work codeLayout
    persoLayout = forWorkspace perso codeLayout
    forWorkspace ws = onWorkspace (name ws)
    tabs = renamed [CutWordsRight 1] $ tabbed shrinkText tabTheme
    tabTheme =
      def
        { activeBorderColor = focusedBorderColor c
        , activeBorderWidth = borderWidth c
        , decoHeight = 20
        }
