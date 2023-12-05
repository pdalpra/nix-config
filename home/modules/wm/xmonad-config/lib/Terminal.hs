module Terminal (setup, runShow) where

import XMonad
import XMonad.Prelude


setup :: XConfig l -> XConfig l
setup c =
  c {terminal = myTerminal}


myTerminal :: String
myTerminal = "alacritty"


runShow :: String -> [String] -> String -> String
runShow name options cmd =
  mconcat
    [myTerminal, " -T ", name, " ", optionsString, " -e sh -c '", cmd, "'"]
  where
    optionsString = mconcat $ intersperse " " options
