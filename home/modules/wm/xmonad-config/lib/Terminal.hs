module Terminal (setup, runShow, term) where

import XMonad
import XMonad.Prelude


setup :: XConfig l -> XConfig l
setup c =
  c {terminal = term}


term :: String
term = "kitty"


runShow :: String -> [String] -> String -> String
runShow name options cmd =
  mconcat
    [term, " -T ", name, " ", optionsString, " -e sh -c '", cmd, "'"]
  where
    optionsString = mconcat $ intersperse " " options
