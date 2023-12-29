{-# OPTIONS_GHC -Wno-missing-signatures #-}
import System.IO (hPutStrLn) 
import XMonad hiding ((|||))
import XMonad.Actions.CopyWindow 
import XMonad.Actions.UpdatePointer 
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks)
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.GridVariants
import XMonad.Layout.LayoutCombinators ((|||))
import XMonad.Layout.Renamed (Rename (Replace), renamed)
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (spawnPipe)

main :: IO ()
main = do
  let runXmobar screen = "xmobar -x " <> screen <> " $XDG_CONFIG_HOME/xmobar/.xmobarrc" 
  xmproc0 <- spawnPipe $ runXmobar "0"
  xmproc1 <- spawnPipe $ runXmobar "1"
  xmonad $
    ewmh
      desktopConfig
        { manageHook = manageDocks <+> manageHook desktopConfig,
          layoutHook = myLayout,
          handleEventHook = handleEventHook desktopConfig,
          workspaces = myWorkspaces,
          borderWidth = myBorderWidth,
          terminal = myTerminal,
          modMask = myModMask,
          normalBorderColor = myNormalBorderColor,
          focusedBorderColor = myFocusedBorderColor,
          logHook =
            dynamicLogWithPP
              xmobarPP
                { ppOutput = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x,
                  ppCurrent = xmobarColor myppCurrent "" . wrap "[" "]", -- Current workspace in xmobar
                  ppVisible = xmobarColor myppVisible "", -- Visible but not current workspace
                  ppHidden = xmobarColor myppHidden "" . wrap "+" "", -- Hidden workspaces in xmobar
                  ppHiddenNoWindows = xmobarColor myppHiddenNoWindows "", -- Hidden workspaces (no windows)
                  ppTitle = xmobarColor myppTitle "" . shorten 80, -- Title of active window in xmobar
                  ppSep = "<fc=#586E75> | </fc>", -- Separators in xmobar
                  ppUrgent = xmobarColor myppUrgent "" . wrap "!" "!", -- Urgent workspace
                  ppExtras = [windowCount], -- # of windows current workspace
                  ppOrder = \(ws : l : t : ex) -> [ws, l] ++ ex ++ [t]
                }
              >> updatePointer (0.25, 0.25) (0.25, 0.25)
        }
      `additionalKeysP` myKeys

myModMask = mod4Mask

myBorderWidth = 0

myNormalBorderColor = "#34495e"

myFocusedBorderColor = "#9d9d9d"

myppCurrent = "#cb4b16"

myppVisible = "#cb4b16"

myppHidden = "#268bd2"

myppHiddenNoWindows = "#93A1A1"

myppTitle = "#FDF6E3"

myppUrgent = "#DC322F"

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myBrowser = "google-chrome-stable"

myTerminal = "alacritty"

appLauncher = "rofi -show combi -modes combi -combi-modes \"window,drun\" -show-icons"

emojiPicker = "rofi -modi emoji -show emoji -emoji-mode copy"

mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

myLayout = avoidStruts (tiled ||| grid ||| bsp ||| simpleTabbed2)
  where
    tiled = renamed [Replace "Tall"] $ mySpacing 6 $ ResizableTall 1 (3 / 100) (1 / 2) []
    grid = renamed [Replace "Grid"] $ spacingRaw True (Border 10 0 10 0) True (Border 0 10 0 10) True $ mySpacing 15 $ Grid (16 / 10)
    bsp = renamed [Replace "BSP"] $ mySpacing 8 emptyBSP
    simpleTabbed2 = renamed [Replace "tab"] $ mySpacing 8 simpleTabbed

myKeys =
  [ ("M-" ++ m ++ k, windows $ f i)
    | (i, k) <- zip myWorkspaces (map show [1 :: Int ..]),
      (f, m) <- [(W.view, ""), (W.shift, "S-"), (copy, "S-C-")]
  ]
    ++ [ ("M-b", spawn myBrowser),
         ("M-/", spawn appLauncher),
         ("M-.", spawn emojiPicker),
         ("<XF86MonBrightnessUp>", spawn "brightnessctl -d intel_backlight set +300"),
         ("<XF86MonBrightnessDown>", spawn "brightnessctl -d intel_backlight set 300-"),
         ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute"),
         ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute"),
         ("<XF86AudioMute>", spawn "amixer sset Master toggle"),
         ("<XF86AudioMicMute>", spawn "amixer sset Capture toggle")
       ]
