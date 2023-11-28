import Control.Monad
import Graphics.X11.ExtraTypes.XF86
import System.Environment
import System.Exit
import System.IO
import XMonad
import XMonad.Actions.GridSelect
import XMonad.Actions.SpawnOn
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce
import qualified Data.Map as M
import qualified XMonad.StackSet as W

-- Define ManageHooks
myManageHook = composeAll [
    isFullscreen --> (doF W.focusDown <+> doFullFloat),
    isDialog --> doCenterFloat,
    appName =? "desktop_window" --> doIgnore,
    manageDocks
    ]

-- Define StartupHook
myStartupHook = do
    setWMName "LG3D"
    spawnOnce "xcompmgr"
    spawnOnce "polybar"
    spawnOnce "volumeicon"
    spawnOnce "nm-applet"
    spawnOnce "redshift"
    spawnOnce "ibus-daemon"
    spawnOnce "feh --bg-scale ~/.xmonad/background.jpg"

-- Define the names of all workspaces
myWorkspaces = ["1-docs", "2-code", "3-code", "4-term", "5-term", "6-sql", "7-chat", "8-mail", "9-vm"]

-- Define Terminal
myTerminal = "gnome-terminal"

-- Define Layout
myLayout =  avoidStruts $ tiled ||| Mirror tiled ||| Full
    where tiled = Tall nmaster delta ratio
          nmaster = 1 -- default number of windows in master screen
          delta = 3/100 -- default percent of resizing panes
          ratio = 3/5 -- default proportion of screen occupied by master pane

-- Define Border
myNormalBorderColor = "#353945"
myFocusedBorderColor = "#ffffff"
myBorderWidth = 1

-- Define ShortCuts
myShortCuts = [
    ((mod4Mask, xK_l), spawn "slock"),
    ((0, xF86XK_MonBrightnessUp), spawn "xbacklight +5"),
    ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -5"),
    -- browser: firefox
    ((mod4Mask, xK_f), spawn "firefox"),
    -- browser: chromium
    ((mod4Mask, xK_c), spawn "google-chrome"),
    -- Mail
    ((mod4Mask, xK_m), spawn "thunderbird"),
    -- Screenshort: scrot
    ((mod4Mask, xK_a), spawn "sleep 0.2; scrot -s -e 'xclip -selection clipboard -t \"image/png\" < $f && rm $f'"),
    ((mod4Mask, xK_s), spawn "sleep 0.2; scrot -s"),
    -- DocViewer: zeal
    ((mod4Mask, xK_d), spawn "zeal"),
    -- E-Book: zathura
    ((mod4Mask, xK_e), spawn "zathura"),
    ((mod4Mask, xK_b), sendMessage ToggleStruts),
    ((mod1Mask, xK_Tab), goToSelected def)
    -- alt+p, use dmenu
    ]


main = do
    xmonad $ ewmh . docks $ desktopConfig {
        manageHook = myManageHook <+> manageHook desktopConfig,
        handleEventHook = handleEventHook desktopConfig,
        layoutHook = avoidStruts $ smartBorders myLayout,
        terminal = myTerminal,
        startupHook = myStartupHook,
        borderWidth = myBorderWidth,
        workspaces = myWorkspaces,
        normalBorderColor = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor
    } `additionalKeys` myShortCuts
