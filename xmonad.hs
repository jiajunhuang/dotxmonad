import Control.Monad
import Graphics.X11.ExtraTypes.XF86
import System.Taffybar.Hooks.PagerHints (pagerHints)
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
    spawnOnce "taffybar"
    spawnOnce "xcompmgr"
    spawnOnce "volumeicon"
    spawnOnce "fcitx"
    spawnOnce "udiskie -aN"
    spawnOnce "nm-applet"
    spawnOnce "sogou-qimpanel"
    spawnOnce "dropbox start"

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

--Define Xmobar color
myXmobarColorfg = "#fdf6e3"
myXmobarColorbg = ""

-- Define Xmonad.Prompt
myXmonadPromptConfig = def {
    position = Top,
    alwaysHighlight = True,
    promptBorderWidth = 0,
    font = "xft:monospace:size=12"
}

-- Define ShortCuts
myShortCuts = [
    ((mod4Mask, xK_l), spawn "slock"),
    ((0, xF86XK_MonBrightnessUp), spawn "xbacklight +20"),
    ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -20"),
    -- browser: google chrome
    ((mod4Mask, xK_c), spawn "google-chrome"),
    -- MySQL: mysql-workbentch
    ((mod4Mask, xK_m), spawn "mysql-workbench"),
    -- E-Mail: geary
    ((mod4Mask, xK_t), spawn "geary"),
    -- Screenshort: scrot
    ((mod4Mask, xK_a), spawn "sleep 0.2; scrot -s -e 'mv $f /data/samba/shots/'"),
    -- DocViewer: zeal
    ((mod4Mask, xK_d), spawn "zeal"),
    ((mod4Mask, xK_b), sendMessage ToggleStruts),
    ((mod1Mask, xK_Tab), goToSelected def),
    ((mod1Mask, xK_p), shellPrompt myXmonadPromptConfig)
    ]


main = do
    spawn "feh --bg-scale ~/.xmonad/background.jpg"
    xmonad $ ewmh $ pagerHints $ desktopConfig {
        manageHook = myManageHook <+> manageDocks <+> manageHook desktopConfig,
        handleEventHook = docksEventHook <+> handleEventHook desktopConfig,
        layoutHook = avoidStruts $ smartBorders myLayout,
        terminal = myTerminal,
        startupHook = myStartupHook,
        borderWidth = myBorderWidth,
        workspaces = myWorkspaces,
        normalBorderColor = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor
    } `additionalKeys` myShortCuts
