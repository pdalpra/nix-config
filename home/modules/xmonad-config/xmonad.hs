import XMonad

main :: IO ()
main =
  xmonad $
    def
      { terminal = myTerminal
      , borderWidth = 2
      }

myTerminal = "alacritty"
