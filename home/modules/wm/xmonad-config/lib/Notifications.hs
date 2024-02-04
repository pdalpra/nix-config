{-# LANGUAGE FlexibleContexts #-}

module Notifications (setup) where

import XMonad
import XMonad.Hooks.UrgencyHook (UrgencyHook (..), withUrgencyHook)
import qualified XMonad.StackSet as W
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run (safeSpawn)


setup :: LayoutClass l Window => XConfig l -> XConfig l
setup = withUrgencyHook LibNotifyUrgencyHook


data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)


instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook window = do
    named <- getName window
    workspace <- withWindowSet findWorkspace
    mapM_ (notify named) workspace
    where
      findWorkspace ws = return (W.findTag window ws)
      notify namedWindow workspace =
        safeSpawn
          "notify-send"
          [ "-u"
          , "low"
          , mconcat ["Workspace", workspace, ":"]
          , mconcat ["See '", show namedWindow, "'"]
          ]
