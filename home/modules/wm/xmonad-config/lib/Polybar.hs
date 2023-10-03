{-# LANGUAGE OverloadedRecordDot #-}

module Polybar (setup) where

import qualified Codec.Binary.UTF8.String as UTF8
import qualified DBus as D
import qualified DBus.Client as D
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Prelude


setup :: IO (XConfig a -> XConfig a)
setup = do
  dbus <- initDbus
  return $ \c -> c {logHook = polybarLogHook dbus <> c.logHook}


initDbus :: IO D.Client
initDbus =
  do
    dbus <- D.connectSession
    _ <- D.requestName dbus busName flags
    return dbus
  where
    busName = D.busName_ "org.xmonad.Log"
    flags =
      [ D.nameAllowReplacement
      , D.nameReplaceExisting
      , D.nameDoNotQueue
      ]


polybarLogHook :: D.Client -> X ()
polybarLogHook dbus =
  dynamicLogWithPP
    def
      { ppOutput = dbusOutput dbus
      , ppOrder = singleton . (!! 1) -- Keep only the layout name
      , ppLayout = colorized green
      }
  where
    colorized color text = mconcat ["%{F", color, "}", text, "%{F-}"]
    green = "#00ff47"


dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
  D.emit dbus $ signal {D.signalBody = signalBody}
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"
    signal = D.signal objectPath interfaceName memberName
    signalBody = [D.toVariant . UTF8.decodeString $ str]
