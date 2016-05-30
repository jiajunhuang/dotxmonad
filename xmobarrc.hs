Config {
        font = "xft:Source Han Sans CN:size=8"
      , bgColor = "#353945"
      , fgColor = "#ffffff"
      , position = TopW L 90
      , sepChar = "%" -- delineator between plugin names and straight text
      , alignSep = "}{" -- separator between left-right alignment
      , template = "%StdinReader% }{ %memory% | %battery% | %date%          "
      , commands = [
      Run StdinReader -- read data from xmonad
      , Run Memory [ "--template" ,"Mem: <usedratio>%"
      , "--Low", "20"
      , "--High", "90"
      , "--low", "green"
      , "--normal", "orange"
      , "--high", "red" ] 10
      , Run Battery [ "--template" , "Bat: <left>%"
      , "--Low", "30"
      , "--High", "90"
      , "--low" , "red"
      , "--normal" , "orange"
      , "--high" , "green"] 600
      , Run Com "date" ["+%a %b %_d %H:%M"] "date" 600
    ]
}
