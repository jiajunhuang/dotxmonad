Config {
   -- appearance
   font = "xft:Source Han Sans CN:size=8"
   , bgColor = "#002b36"
   , fgColor = "#fdf6e3"
   , position = TopW L 90

   -- layout
   , sepChar = "%" -- delineator between plugin names and straight text
   , alignSep = "}{" -- separator between left-right alignment
   , template = " %StdinReader% }{ %memory% | %battery% | %date%      "
   , commands = [
   -- read data from xmonad
   Run StdinReader

   -- battery monitor
   , Run Battery [ "--template" , "Bat: <acstatus>"
   , "--Low"
   , "30" -- units: %
   , "--High"
   , "90" -- units: %
   , "--low"
   , "red"
   , "--normal"
   , "orange"
   , "--high"
   , "green"
   , "--" -- battery specific options -- discharging status
   , "-o"
   , "<left>%"
   ] 100

    -- memory usage monitor
    , Run Memory [ "--template" ,"Mem: <usedratio>%"
    , "--Low"      , "20"        -- units: %
    , "--High"     , "90"        -- units: %
    , "--low"      , "green"
    , "--normal"   , "orange"
    , "--high"     , "red"
    ] 100

   -- time and date indicator
   --   (%F = y-m-d date, %a = day of week, %T = h:m:s time)
   , Run Date "%F (%a) %H:%M" "date" 100
   ]
   }
