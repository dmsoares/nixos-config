import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab

main :: IO ()
main = xmonad $ def
    { modMask = mod4Mask  -- Rebind Mod to the Super key
    , terminal = myTerminal
    }
    `additionalKeysP`
    [ ("M-b", spawn myBrowser)
    , ("M-/", spawn appLauncher)
    , ("M-.", spawn emojiPicker)
    ]

myBrowser = "google-chrome-stable"
myTerminal = "alacritty"
appLauncher  = "rofi -modi drun,ssh,window -show drun -show-icons"
emojiPicker  = "rofi -modi emoji -show emoji -emoji-mode copy"