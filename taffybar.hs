import System.Information.CPU2
import System.Information.Memory
import System.Taffybar
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.MPRIS
import System.Taffybar.SimpleClock
import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.Weather
import System.Taffybar.Widgets.PollingBar
import System.Taffybar.Widgets.PollingGraph

memCallback = do
    mi <- parseMeminfo
    return [memoryUsedRatio mi]

memCfg = defaultGraphConfig {
    graphDataColors = [(1, 0, 0, 1)],
    graphLabel = Just "mem"
}

cpuCfg = defaultGraphConfig {
    graphDataColors = [
    (0, 1, 0, 1),
    (1, 0, 1, 0.5)
    ],
    graphLabel = Just "cpu"
}

clock = textClockNew Nothing "<span fgcolor='orange'>%m/%d/%y %H:%M</span>" 1
pager = taffyPagerNew defaultPagerConfig
note = notifyAreaNew defaultNotificationConfig
mpris = mprisNew defaultMPRISConfig
mem = pollingGraphNew memCfg 1 memCallback
cpu = pollingGraphNew cpuCfg 1 $ getCPULoad "cpu"
tray = systrayNew

main = do
    defaultTaffybar defaultTaffybarConfig {
        startWidgets = [pager, note],
        endWidgets = [tray, clock, mem, cpu, mpris]
    }
