{-# LANGUAGE LambdaCase #-}

import Data.Foldable (traverse_)
import Data.Map qualified as M
import System.IO (hPutStrLn)
import XMonad hiding ((|||))
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS (nextScreen, shiftNextScreen, toggleWS)
import XMonad.Actions.MouseResize (mouseResize)
import XMonad.Actions.SpawnOn
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WithAll (killAll)
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks)
import XMonad.Layout ((|||))
import XMonad.Layout.Grid
import XMonad.Layout.MultiToggle ((??))
import XMonad.Layout.MultiToggle qualified as MT
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders (smartBorders, withBorder)
import XMonad.Layout.Renamed (Rename (..), renamed)
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowArranger (windowArrange)
import XMonad.StackSet qualified as W
import XMonad.Util.Cursor (setDefaultCursor)
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
                    { manageHook = myManageHook
                    , startupHook = myStartupHook
                    , layoutHook = myLayoutHook
                    , handleEventHook = handleEventHook desktopConfig
                    , workspaces = myRegularWorkspaces <> myExtraWorkspaces
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
                                , ppOrder = \case
                                    (ws : l : _ : ex) -> [ws, l] ++ ex
                                    _ -> []
                                }
                            >> updatePointer (0, 0) (0, 0)
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

myRegularWorkspaces :: [String]
myRegularWorkspaces = show <$> [1 :: Int .. 9]

myExtraWorkspaces :: [String]
myExtraWorkspaces = ["T"]

clickable :: String -> String
clickable ws = case M.lookup ws myWorkspaceIndices of
    Just i -> "<action=xdotool key super+" ++ show (i :: Int) ++ ">" ++ ws ++ "</action>"
    _ -> ws
  where
    myWorkspaceIndices = M.fromList $ zip myRegularWorkspaces [1 .. 9]

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myBrowser :: String
myBrowser = "google-chrome-stable --high-dpi-support=1 --force-device-scale-factor=1.5"

myTerminal :: String
myTerminal = "alacritty"

appLauncher :: String
appLauncher = "rofi -show combi -modes combi -combi-modes \"window,drun\" -show-icons"

emojiPicker :: String
emojiPicker = "rofi -modi emoji -show emoji -emoji-mode copy"

myKeys :: [(String, X ())]
myKeys =
    [("M-" ++ m ++ k, windows $ f i) | (i, k) <- zip myRegularWorkspaces (map show [1 :: Int ..]), (f, m) <- [(W.greedyView, ""), (W.shift, "S-"), (copy, "S-C-")]]
        ++ [ ("M-<Return>", spawn myTerminal)
           , ("M-t", toggleTerminalWS)
           , ("M-S-t", windows $ W.shift "T")
           , ("M-r", withFocused $ windows . W.sink)
           , ("M-S-m", windows W.swapMaster)
           , ("M-y", nextScreen)
           , ("M-S-y", shiftNextScreen)
           , ("M-s", namedScratchpadAction myScratchPads "music")
           , ("M-d", namedScratchpadAction myScratchPads "todo")
           , ("M-g", spawn "gscreenshot -s -c")
           , ("M-S-g", spawn "gscreenshot")
           , ("M-v", spawn "peek")
           , ("M-o", toggleWS)
           , ("M-b", spawn myBrowser)
           , ("M-f", spawn "thunar")
           , ("M-p", spawn appLauncher)
           , ("M-S-r", spawn "xrandr --output HDMI-1 --off && xrandr --output HDMI-1 --auto &&  xrandr --output HDMI-1 --above eDP-1")
           , ("M-S-p", spawn emojiPicker)
           , ("M-u", spawn "emacsclient -c -a 'emacs'")
           , ("M-/", spawn "emacsclient --eval \"(emacs-everywhere)\"")
           , ("M-S-u", spawn "zeditor")
           , ("M-<Escape>", spawn "slock")
           , ("M-q", kill1)
           , ("M-S-q", killAll)
           , ("M-<Tab>", sendMessage NextLayout)
           , ("M-<Space>", sendMessage (MT.Toggle NBFULL))
           , ("<XF86MonBrightnessUp>", spawn "brightnessctl set 5%+")
           , ("<XF86MonBrightnessDown>", spawn "brightnessctl set 5%-")
           , ("<XF86AudioLowerVolume>", spawn lowerVolume)
           , ("<XF86AudioRaiseVolume>", spawn raiseVolume)
           , ("<XF86AudioMute>", spawn "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")
           , ("S-<XF86AudioLowerVolume>", spawn "amixer set Capture 5%- unmute")
           , ("S-<XF86AudioRaiseVolume>", spawn "amixer set Capture 5%+ unmute")
           , ("S-<XF86AudioMute>", spawn "amixer sset Capture toggle")
           ]
  where
    unmute = "wpctl set-mute @DEFAULT_AUDIO_SINK@ 0 && "
    changeVolume direction = "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%" <> direction
    raiseVolume = unmute <> changeVolume "+"
    lowerVolume = changeVolume "-"
    toggleTerminalWS = do
        tag <- W.currentTag <$> gets windowset
        if tag == "T"
            then toggleWS
            else windows (W.greedyView "T") >> sendMessage (JumpToLayout "Grid")

myStartupHook :: X ()
myStartupHook =
    composeAll
        [ spawn "feh --bg-fill --auto-zoom ~/.fehbg"
        , spawn "emacs --daemon"
        , spawn "blueman-applet"
        , spawnOn "T" myTerminal
        , setDefaultCursor xC_left_ptr
        ]

rectCentered :: Rational -> W.RationalRect
rectCentered percentage = W.RationalRect offset offset percentage percentage
  where
    offset = (1 - percentage) / 2

myManageHook :: ManageHook
myManageHook =
    composeAll
        [ className =? ".gscreenshot-wrapped" --> defaultFloating
        , className =? ".blueman-manager-wrapped" --> customFloating (rectCentered 0.5)
        , className =? "pavucontrol" --> customFloating (rectCentered 0.5)
        , className =? "peek" --> defaultFloating
        , manageSpawn
        , manageDocks
        , namedScratchpadManageHook myScratchPads
        ]

myScratchPads :: [NamedScratchpad]
myScratchPads =
    [ NS "music" "spotify" (className =? "Spotify") (customFloating $ rectCentered 0.8)
    , NS "todo" "emacsclient -c -a 'emacs' -F '((name . \"emacs-todo\"))' ~/Documents/todo.md" (title =? "emacs-todo") nonFloating
    ]

myLayoutHook =
    smartBorders
        . avoidStruts
        . mouseResize
        . windowArrange
        $ MT.mkToggle (NBFULL ?? NOBORDERS ?? MT.EOT) layouts
  where
    layouts =
        withBorder myBorderWidth tall ||| mirroredTall ||| grid ||| threeCol

    tall =
        renamed [Replace "Tall"] $
            ResizableTall 1 (3 / 100) (1 / 2) []

    mirroredTall =
        renamed [Replace "Tall(M)"] $
            Mirror $
                ResizableTall 1 (3 / 100) (1 / 2) []

    grid = Grid

    threeCol = ThreeColMid 1 (3 / 100) (1 / 2)
