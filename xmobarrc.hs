Config {
    font = "xft:Source Han Sans CN:size=8",
    additionalFonts = [],
    borderColor = "black",
    border = TopB,
    textOffset = -1,
    iconOffset = -1,
    lowerOnStart = True,
    pickBroadest = False,
    persistent = False,
    hideOnStart = False,
    iconRoot = ".",
    allDesktops = False,
    overrideRedirect = True,
    alpha = 255,
    bgColor = "#353945",
    fgColor = "#ffffff",
    position = TopW L 90,
    sepChar = "%", -- delineator between plugin names and straight text
    alignSep = "}{", -- separator between left-right alignment
    template = "%StdinReader% }{ %memory% | %date%          ",
    commands = [
        Run StdinReader, -- read data from xmonad
        Run Memory [
            "--template" ,"Mem: <usedratio>%",
            "--Low", "20",
            "--High", "90",
            "--low", "green",
            "--normal", "orange",
            "--high", "red"
        ] 60,
        Run Com "date" ["+%a %b %_d %H:%M"] "date" 60
    ]
}
