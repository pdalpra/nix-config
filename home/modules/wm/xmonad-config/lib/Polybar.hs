module Polybar (initDbus, hook) where

import qualified Codec.Binary.UTF8.String as UTF8
import qualified DBus as D
import qualified DBus.Client as D
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Util.NamedScratchpad (scratchpadWorkspaceTag)


initDbus :: IO D.Client
initDbus =
  do
    dbus <- D.connectSession
    D.requestName dbus busName flags
    return dbus
  where
    busName = D.busName_ "org.xmonad.Log"
    flags = [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]


hook :: D.Client -> X ()
hook dbus =
  dynamicLogWithPP
    def
      { ppOutput = dbusOutput dbus
      , ppCurrent = colorized blue
      , ppVisible = colorized gray
      , ppUrgent = colorized orange
      , ppHidden = colorized gray
      , ppHiddenNoWindows = colorized red
      , ppSep = " ||| "
      , ppLayout = colorized green
      , ppTitle = mempty
      }
  where
    colorized color wp
      | wp /= scratchpadWorkspaceTag = wrap ("%{F" <> color <> "}") "%{F-}" wp
      | otherwise = mempty
    green = "#00ff47"
    blue = "#2e9afe"
    red = "#722222"
    gray = "#7f7f7f"
    orange = "#ea4300"


dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
  D.emit dbus $ signal {D.signalBody = signalBody}
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"
    signal = D.signal objectPath interfaceName memberName
    signalBody = [D.toVariant $ UTF8.decodeString str]
