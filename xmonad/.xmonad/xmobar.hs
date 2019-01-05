Config { font              = "xft:xos4 inconsolata:pixelsize=12:antialias=true:hinting=true"
       , additionalFonts   = [ "xft:FontAwesome:pixelsize=14:antialias=true:hinting=true" ]
       , borderColor = "rgba(255, 255, 255, 0)"
       , bgColor = "#fdd6b5"
       , fgColor = "#1f1f1f"
       , position = BottomW R 28
       , allDesktops = True
       , overrideRedirect = True
       , alpha = 255
       , iconOffset = 2
       , lowerOnStart = True
       , pickBroadest = False
       , persistent = False
       , hideOnStart = False
       , iconRoot = "."
       , commands = [ Run BatteryN ["BAT0"]  ["-t", "<acstatus>", "--", "-O", "⬘ <left>", "-o", "⬙ <left>"] 60 "bat"
                    , Run Date "%a %I:%M%P (Local)" "Local" 10
                    , Run DateZone "%a %I:%M%P (AEST)" "en_US.UTF-8" "Australia/Sydney" "Sydney" 10
                    , Run DateZone "%a %I:%M%P (PDT)" "en_US.UTF-8" "America/Los_Angeles" "Los_Angeles" 10
                    , Run DateZone "%a %I:%M%P s(EDT)" "en_US.UTF-8" "America/New_York" "New_York" 10
                    , Run DateZone "%a %I:%M%P (IST)" "en_US.UTF-8" "Europe/Dublin" "Dublin" 10
                    , Run DynNetwork     [ "--template" , "⬘ <tx>kb/s  ⬙ <rx>kb/s"] 60
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "}{  %dynnetwork%      |||      %Local%    -    %Sydney%     -    %Los_Angeles%    -    %New_York%    -    %Dublin%  "
       }
