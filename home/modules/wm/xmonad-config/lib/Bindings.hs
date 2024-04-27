{-# LANGUAGE OverloadedRecordDot #-}

module Bindings where

import Data.Ratio ((%))
import qualified Terminal
import XMonad
import XMonad.Hooks.ManageHelpers (doRectFloat)
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Prelude
import qualified XMonad.StackSet as W
import XMonad.Util.NamedActions


setup :: XConfig l -> XConfig l
setup c =
  addDescrKeys'
    ((myModMask .|. shiftMask, xK_slash), showKeybindings helpTitle)
    myKeys
    $ c
      { modMask = myModMask
      , manageHook = setHelpGeometry <+> c.manageHook
      , logHook = closeHelpOnFocusLost <+> c.logHook
      }
  where
    myModMask = XMonad.modMask def
    helpTitle = "Keybindings"
    query = title =? helpTitle
    setHelpGeometry = query --> doRectFloat (W.RationalRect (1 % 3) (1 % 4) (1 % 3) (1 % 2))
    closeHelpOnFocusLost =
      withUnfocused $ \w -> runQuery query w --> withDisplay (io . void . flip killClient w)


showKeybindings :: String -> [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings windowTitle bindings =
  addName "Show keybindings"
    . spawn
    $ Terminal.runShow windowTitle ["-o font.size=6"] text
  where
    text = mconcat ["echo \"", unlines $ showKm bindings, "\" | pr -2 -t | less"]

{- FOURMOLU_DISABLE -}
myKeys :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
myKeys c =
  foldl
    (^++^)
    []
    [ keySet
        "Apps"
        [ key "Apps"     (modm .|. shiftMask, xK_p)      $ spawn "rofi -show drun"
        , key "Terminal" (modm .|. shiftMask, xK_Return) $ spawn c.terminal
        , key "Emoji"    (modm .|. shiftMask, xK_e)      $ spawn "rofimoji"
        , key "Weather"  (modm .|. shiftMask, xK_w)      $ spawn "rofi -show window"
        ]
    , keySet
        "Layouts"
        [ key "Next layout"          (modm,               xK_space) $ sendMessage NextLayout
        , key "Reset to base layout" (modm .|. shiftMask, xK_space) $ setLayout c.layoutHook
        , key "Toggle fullscreen"    (modm,               xK_f)     $ sendMessage $ Toggle NBFULL
        ]
    , keySet
        "System"
        [ key "Power menu"             (modm .|. shiftMask, xK_q) $ spawn "rofi -show pm"
        , key "Lock screen"            (modm .|. shiftMask, xK_l) $ spawn "xset s activate"
        , key "Screenshot (selection)" (modm,               xK_s) $ spawn "scrot -s -F $HOME/Pictures/screenshots/%d-%m-%Y_%H:%M.png"
        , key "Screenshot (full)"      (modm .|. shiftMask, xK_s) $ spawn "scrot -F $HOME/Pictures/screenshots/%d-%m-%Y_%H:%M.png"
        , key "Restart XMonad"         (modm,               xK_q) $ spawn "xmonad --restart"
        ]
    , keySet
        "Windows => switch"
        [ key "Previous"      (modm .|. shiftMask,                 xK_Left)   $ windows W.focusUp
        , key "Next"          (modm .|. shiftMask,                 xK_Right)  $ windows W.focusDown
        , key "Focus main"    (modm,                               xK_m)      $ windows W.focusMaster
        , key "Switcher"      (modm .|. shiftMask,                 xK_Tab)    $ spawn "rofi -show window"
        , key "Close focused" (modm,                               xK_Escape) kill
        , key "Close any"     (modm .|. controlMask .|. shiftMask, xK_c)      $ spawn "xkill"
        ]
    , keySet
        "Windows => move"
        [ key "Swap with main"               (modm,               xK_Return) $ windows W.swapMaster
        , key "Swap with next"               (modm .|. shiftMask, xK_Down)   $ windows W.swapDown
        , key "Swap with previous"           (modm .|. shiftMask, xK_Up)     $ windows W.swapUp
        , key "Tile focused floating window" (modm,               xK_t)      $ withFocused $ windows . W.sink
        ]
    , keySet
        "Windows => resize"
        [ key "Shrink main area"             (modm,               xK_minus)  $ sendMessage Shrink
        , key "Expand main area"             (modm .|. shiftMask, xK_equal)  $ sendMessage Expand
        , key "Add window to main area"      (modm,               xK_comma)  $ sendMessage $ IncMasterN 1
        , key "Remove window from main area" (modm,               xK_period) $ sendMessage $ IncMasterN (-1)
        ]
    , keySet "Workspaces" switchOrMoveOnWorkspaces
    , keySet "Screens"    switchOrMoveOnScreens
    ]
  where
    modm = c.modMask
    key name binding action = (binding, addName name action)
    keySet setName ks = subtitle setName : ks
    switchOrMoveOnWorkspaces =
      [ key (mconcat [action, " workspace ", show i]) (m .|. modm, k) $ windows $ f i
      | (f, m) <- [(W.greedyView, noModMask), (W.shift, shiftMask)]
      , let action = if m == shiftMask then "Move to" else "Switch to"
      , (i, k) <- zip c.workspaces [xK_1 .. xK_9]
      ]
    switchOrMoveOnScreens =
      [ key (mconcat [action, " screen ", show screen]) (m .|. modm, k) (screenWorkspace screen >>= flip whenJust (windows . f))
      | (f, m) <- [(W.view, mod2Mask), (W.shift, mod2Mask .|. shiftMask)]
      , let action = if m == mod2Mask .|. shiftMask then "Move to" else "Switch to" 
      , (k, screen) <- zip [xK_1, xK_2] [0 ..]
      ]
{- FOURMOLU_ENABLE -}
