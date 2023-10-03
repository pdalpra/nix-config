{-# LANGUAGE OverloadedRecordDot #-}

module KeyboardLayout (setup) where

import XMonad
import XMonad.Actions.KeyRemap
import XMonad.Prelude
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (runProcessWithInput)


setup :: XConfig a -> XConfig a
setup c =
  c
    { handleEventHook = remapKeysOnLayoutChange <> c.handleEventHook
    , startupHook = c.startupHook >> setDefault
    }
    `additionalKeys` buildKeyRemapBindings allRemaps


allRemaps :: [KeymapTable]
allRemaps =
  [emptyKeyRemap, qwertyToBepo]


setDefault :: X ()
setDefault =
  setDefaultKeyRemap emptyKeyRemap allRemaps


remapKeysOnLayoutChange :: Event -> X All
remapKeysOnLayoutChange (MappingNotifyEvent {}) =
  do
    layout <- words <$> runProcessWithInput "xkblayout-state" ["print", "%s %v"] ""
    _ <- setKeyRemap $ remap layout
    mempty
  where
    remap ["us"] = emptyKeyRemap
    remap ["fr", "bepo"] = emptyKeyRemap
    remap _ = emptyKeyRemap
remapKeysOnLayoutChange _ = mempty


data KeyboardLayout = KeyboardLayout
  { base :: String
  , shift :: String
  }


qwertyLayout :: KeyboardLayout
qwertyLayout =
  KeyboardLayout
    { base = "`1234567890-=qwertyuiop[]\\asdfghjkl;'zxcvbnm,./"
    , shift = "~!@#$%^&*()_+QWERTYUIOP{}|ASDFGHJKL:\"ZXCVBNM<>?"
    }


bepoLayout :: KeyboardLayout
bepoLayout =
  KeyboardLayout
    { base = "$\"«»()@+-/*=bépoè^vdljzauie,ctsrnmàyx.k'qghfwç%"
    , shift = "#1234567890°BÉPOÈ!VDLJZAUIE;CTSRNMÀYX:K?QGHFWÇ`"
    }


qwertyToBepo :: KeymapTable
qwertyToBepo = makeKeymapTable qwertyLayout bepoLayout


makeKeymapTable :: KeyboardLayout -> KeyboardLayout -> KeymapTable
makeKeymapTable from to =
  KeymapTable $ remap base noModMask ++ remap shift shiftMask
  where
    remap getChars mask = zipWith (setMask mask) (toKeySyms getChars from) (toKeySyms getChars to)
    toKeySyms getChars layout = map (fromIntegral . fromEnum) $ getChars layout :: [KeySym]
    setMask mask fromChar toChar = ((mask, fromChar), (mask, toChar))
