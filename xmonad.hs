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
import Graphics.X11.ExtraTypes.XF86
import System.IO
import System.Exit
import qualified XMonad.StackSet as W
import qualified Data.Map as M

--ManageHooks
myManageHook = composeAll [
    isFullscreen --> (doF W.focusDown <+> doFullFloat)
    , resource =? "desktop_window" --> doIgnore
    , resource =? "Dialog" --> doFloat
    ]

-- Define the names of all workspaces
myWorkspaces = ["1-docs", "2-work", "3-code", "4-other"] ++ map show[5..9]

-- Define Terminal
myTerminal = "termite"

-- Define Layout
myLayout =  tiled1 ||| Mirror tiled1 ||| Full
    where tiled1 = Tall nmaster1 delta ratio
          nmaster1 = 1
          nmaster2 = 2
          ratio = 3/5
          delta = 3/100

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
        , ((mod4Mask, xK_g), spawn "chromium")
        , ((mod4Mask, xK_e), spawn "zathura")
        , ((mod4Mask, xK_b), sendMessage ToggleStruts)
        ]
