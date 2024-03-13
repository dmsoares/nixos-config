import Data.Foldable (traverse_)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import System.IO (hPutStrLn)
import XMonad hiding ((|||))
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS (nextScreen, shiftNextScreen, toggleWS)
import XMonad.Actions.MouseResize (mouseResize)
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WithAll (killAll)
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks)
import XMonad.Layout ((|||))
import XMonad.Layout.LimitWindows (limitWindows)
import XMonad.Layout.MultiToggle ((??))
import XMonad.Layout.MultiToggle qualified as MT
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders (smartBorders, withBorder)
import XMonad.Layout.Renamed (Rename (..), renamed)
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowArranger (windowArrange)
import XMonad.Layout.WindowNavigation (windowNavigation)
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (spawnPipe)

main :: IO ()
main =
  let runXmobar screen = spawnPipe $ "xmobar -x " <> screen
   in do
        xmprocs <- traverse runXmobar ["1", "2"]
        xmonad
          . ewmh
          $ desktopConfig
            { manageHook = manageDocks <+> manageHook desktopConfig
            , startupHook = myStartupHook
            , layoutHook = myLayoutHook
            , handleEventHook = handleEventHook desktopConfig
            , workspaces = myWorkspaces
            , borderWidth = myBorderWidth
            , terminal = myTerminal
            , modMask = myModMask
            , normalBorderColor = myNormalBorderColor
            , focusedBorderColor = myFocusedBorderColor
            , logHook =
                dynamicLogWithPP
                  xmobarPP
                    { ppOutput = \x -> traverse_ (`hPutStrLn` x) xmprocs
                    , ppCurrent = xmobarColor myppCurrent "" -- Current workspace in xmobar
                    , ppVisible = xmobarColor myppVisible "" -- Visible but not current workspace
                    , ppHidden = xmobarColor myppHidden "" -- Hidden workspaces in xmobar
                    , ppHiddenNoWindows = xmobarColor myppHiddenNoWindows "" . clickable -- Hidden workspaces (no windows)
                    , ppTitle = xmobarColor myppTitle "" . shorten 80 -- Title of active window in xmobar
                    , ppSep = "<fc=#586E75> | </fc>" -- Separators in xmobar
                    , ppUrgent = xmobarColor myppUrgent "" . wrap "!" "!" -- Urgent workspace
                    , ppExtras = [windowCount] -- # of windows current workspace
                    , ppOrder = \(ws : l : _ : ex) -> [ws, l] ++ ex
                    }
                  >> updatePointer (0.25, 0.25) (0.25, 0.25)
            }
          `additionalKeysP` myKeys

myModMask :: KeyMask
myModMask = mod4Mask

myBorderWidth :: Dimension
myBorderWidth = 5

myNormalBorderColor :: String
myNormalBorderColor = "#282828"

myFocusedBorderColor :: String
myFocusedBorderColor = "#93a1a1"

myppCurrent :: String
myppCurrent = "#cb4b16"

myppVisible :: String
myppVisible = "#b16286"

myppHidden :: String
myppHidden = "#268bd2"

myppHiddenNoWindows :: String
myppHiddenNoWindows = "#93A1A1"

myppTitle :: String
myppTitle = "#FDF6E3"

myppUrgent :: String
myppUrgent = "#DC322F"

myWorkspaces :: [String]
myWorkspaces = show <$> [1 :: Int .. 9]

scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS "music" "spotify" (className =? "Spotify") (customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3))
  , NS "todo" "emacsclient -c -a 'emacs' -F '((name . \"emacs-todo\"))' ~/Documents/todo.md" (title =? "emacs-todo") defaultFloating
  ]

clickable :: [Char] -> [Char]
clickable ws = "<action=xdotool key super+" ++ show (i :: Int) ++ ">" ++ ws ++ "</action>"
 where
  i = fromJust $ M.lookup ws myWorkspaceIndices
  myWorkspaceIndices = M.fromList $ zip myWorkspaces [1 .. 9]

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myBrowser :: String
myBrowser = "google-chrome-stable"

myTerminal :: String
myTerminal = "alacritty"

appLauncher :: String
appLauncher = "rofi -show combi -modes combi -combi-modes \"window,drun\" -show-icons"

emojiPicker :: String
emojiPicker = "rofi -modi emoji -show emoji -emoji-mode copy"

myKeys :: [(String, X ())]
myKeys =
  [("M-" ++ m ++ k, windows $ f i) | (i, k) <- zip myWorkspaces (map show [1 :: Int ..]), (f, m) <- [(W.greedyView, ""), (W.shift, "S-"), (copy, "S-C-")]]
    ++ [ ("M-<Return>", spawn myTerminal)
       , ("M-S-m", windows W.swapMaster)
       , ("M-y", nextScreen)
       , ("M-S-y", shiftNextScreen)
       , ("M-s", namedScratchpadAction scratchpads "music")
       , ("M-d", namedScratchpadAction scratchpads "todo")
       , ("M-o", toggleWS)
       , ("M-b", spawn myBrowser)
       , ("M-f", spawn "thunar")
       , ("M-p", spawn appLauncher)
       , ("M-S-p", spawn emojiPicker)
       , ("M-u", spawn "emacsclient -c -a 'emacs'")
       , ("M-S-u", spawn "code")
       , ("M-<Escape>", spawn "slock")
       , ("M-q", kill1)
       , ("M-S-q", killAll)
       , ("M-<Tab>", sendMessage NextLayout)
       , ("M-<Space>", sendMessage (MT.Toggle NBFULL))
       , ("<XF86MonBrightnessUp>", spawn "brightnessctl set 5%+")
       , ("<XF86MonBrightnessDown>", spawn "brightnessctl set 5%-")
       , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
       , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
       , ("<XF86AudioMute>", spawn "amixer sset Master toggle")
       , ("S-<XF86AudioLowerVolume>", spawn "amixer set Capture 5%- unmute")
       , ("S-<XF86AudioRaiseVolume>", spawn "amixer set Capture 5%+ unmute")
       , ("S-<XF86AudioMute>", spawn "amixer sset Capture toggle")
       ]

myStartupHook :: X ()
myStartupHook = do
  spawn "feh --bg-fill --auto-zoom ~/.fehbg"
  spawn "emacs --daemon"
  spawn "blueman-applet"

myLayoutHook =
  smartBorders
    . avoidStruts
    . mouseResize
    . windowArrange
    $ MT.mkToggle (NBFULL ?? NOBORDERS ?? MT.EOT) myDefaultLayout
 where
  myDefaultLayout =
    withBorder myBorderWidth tall
      ||| threeCol
      ||| threeRow
  tall =
    renamed [Replace "tall"]
      . limitWindows 5
      . windowNavigation
      $ ResizableTall 1 (3 / 100) (1 / 2) []

  threeCol =
    renamed [Replace "threeCol"]
      . limitWindows 7
      . windowNavigation
      $ ThreeCol 1 (3 / 100) (1 / 2)

  threeRow =
    renamed [Replace "threeRow"]
      . limitWindows 7
      . windowNavigation
      . Mirror
      $ ThreeCol 1 (3 / 100) (1 / 2)