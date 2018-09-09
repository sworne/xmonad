Config { font = "-misc-fixed-*-*-*-*-13-*-*-*-*-*-*-*"
       , bgColor = "#1f1f1f"
       , fgColor = "#ffc500"
       , position = TopW L 85
       , commands = [ Run DateZone "%H:%M %b %d" "fr_FR.UTF-8" "UTC" "date" 10,
                      Run DateZone "%H:%M %b %d" "fr_FR.UTF-8" "UTC" "date" 10]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%date% %date%"
       }
