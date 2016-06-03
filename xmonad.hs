import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.NoBorders
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Actions.SpawnOn
import XMonad.Actions.GridSelect
import Graphics.X11.ExtraTypes.XF86
import System.IO
import System.Exit
import qualified XMonad.StackSet as W
import qualified Data.Map as M

--ManageHooks
myManageHook = composeAll [
    isFullscreen --> (doF W.focusDown <+> doFullFloat)
    , isDialog --> doFloat
    , appName =? "netease-cloud-music" --> doShift "6-music"
    , appName =? "xchat" --> doShift "7-chat"
    , appName =? "VirtualBox" --> doShift "8-vm"
    , appName =? "desktop_window" --> doIgnore
    ]

-- Define the names of all workspaces
myWorkspaces = ["1-docs", "2-code", "3-code", "4-code", "5-mail", "6-music", "7-chat", "8-vm", "9-others"]

-- Define Terminal
myTerminal = "gnome-terminal"

-- Define Layout
myLayout =  tiled ||| Full
    where tiled = Tall nmaster delta ratio
          nmaster = 1 -- default number of windows in master screen
          delta = 3/100 -- default percent of resizing panes
          ratio = 3/5 -- default proportion of screen occupied by master pane

-- Define BorderColor
myNormalBorderColor = "#353945"
myFocusedBorderColor = "#ffffff"

main = do
    spawn "feh --bg-scale ~/.xmonad/background.jpg"
    xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc.hs"
    xmonad $ ewmh defaultConfig {
        manageHook = myManageHook <+> manageDocks <+> manageHook defaultConfig
        , handleEventHook = docksEventHook <+> handleEventHook defaultConfig
        , layoutHook = avoidStruts $ smartBorders $ myLayout
        , terminal = myTerminal
        , logHook = do
            takeTopFocus
            dynamicLogWithPP $ xmobarPP {
                ppOutput = hPutStrLn xmproc
                , ppTitle = xmobarColor "#fdf6e3" "" . shorten 60
                , ppLayout = const "" -- to disable the layout info on xmobar
            }
            , borderWidth = 1
            , workspaces = myWorkspaces
            , normalBorderColor = myNormalBorderColor
            , focusedBorderColor = myFocusedBorderColor
            } `additionalKeys` [
                ((mod4Mask, xK_l), spawn "slock")
                , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight +20")
                , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -20")
                , ((mod4Mask, xK_c), spawn "chromium")
                , ((mod4Mask, xK_e), spawn "zathura")
                , ((mod4Mask, xK_m), spawn "netease-cloud-music")
                , ((mod4Mask, xK_t), spawn "touchpad_toggle.sh")
                , ((mod4Mask, xK_b), sendMessage ToggleStruts)
                , ((mod4Mask, xK_Tab), goToSelected defaultGSConfig)
                ]
