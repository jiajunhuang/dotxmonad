Config {
        font = "xft:WenQuanYi Zen Hei:size=10"
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
      , "--high", "red" ] 60
      , Run Battery [ "--template" , "Bat: <left>%"
      , "--Low", "30"
      , "--High", "90"
      , "--low" , "red"
      , "--normal" , "orange"
      , "--high" , "green"] 60
      , Run Com "date" ["+%a %b %_d %H:%M"] "date" 60
    ]
}
