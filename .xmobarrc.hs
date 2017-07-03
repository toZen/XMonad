Config { 
        -- appearance
                                 font             =   "xft:TerminusRe33:size=10:normal:antialias=true"
                               , additionalFonts  =  ["xft:TerminusRe33:size=10:antialise=true:hinting=true"]
                               , iconRoot         =   "/home/tozen/.xmonad/.icons"
                               , bgColor          =   "#1c1c1c"
                               , fgColor          =   "#c0c0c0"
                               , borderColor      =   "#646464"
                               , border           =   BottomB
                               , pickBroadest     =   False
                               , hideOnStart      =   False
                               , lowerOnStart     =   True
                               , allDesktops      =   True
                               , overrideRedirect =   True
                               , persistent       =   True
                               , position         =   Top
                               , alpha            =   200
                               , textOffset       =   12
                               , iconOffset       =   7
                                 commands         = 
        -- network activity monitor
        [ Run DynNetwork       [ "--template"     ,   "<icon=uparrow7.xbm/> <tx>kB/s <icon=downarrow7.xbm/> <rx>kB/s"
                               , "--Low"          ,   "1000"       -- units: kB/s
                               , "--High"         ,   "5000"       -- units: kB/s
                               , "--low"          ,   "#d2d4dc"
                               , "--normal"       ,   "#d2d4dc"
                               , "--high"         ,   "#d2d4dc"
                               ] 10

        -- cpu activity monitor
        , Run MultiCpu         [ "--template"     ,   "<icon=cpu1.xbm/><fc=#d2d4dc> </fc><total0>%"
                               , "--Low"          ,   "50"         -- units: %
                               , "--High"         ,   "85"         -- units: %
                               , "--low"          ,   "#d2d4dc"
                               , "--normal"       ,   "#d2d4dc"
                               , "--high"         ,   "#fd0537"
                               ] 10

        -- cpu core temperature monitor
        , Run CoreTemp         [ "--template"     ,   "<icon=temp1.xbm/><fc=#d2d4dc> </fc><core0>°C"
                               , "--Low"          ,   "2"         -- units: °C
                               , "--High"         ,   "80"        -- units: °C
                               , "--low"          ,   "#d2d4dc"
                               , "--normal"       ,   "#d2d4dc"
                               , "--high"         ,   "fd0537"
                               ] 50
                          
        -- memory usage monitor
        , Run Memory           [ "--template"     ,   "<icon=mem1.xbm/><fc=#d2d4dc> </fc><usedratio>%"
                               , "--Low"          ,   "20"        -- units: %
                               , "--High"         ,   "90"        -- units: %
                               , "--low"          ,   "#d2d4dc"
                               , "--normal"       ,   "#d2d4dc"
                               , "--high"         ,   "#fd0537"
                               ] 10
        
        -- battery monitor
        , Run BatteryP         ["BAT0"]
                               [ "-t"             ,   "<acstatus> <left>%"
		 				                   , "-L"             ,   "10"
		 				                   , "-H"             ,   "60"
		 				                   , "-l"             ,   "#ff0000"
							                 , "-h"             ,   "#c0c0c0"
		 				                   , "--"
							                 , "-O"             ,   "<icon=ac10.xbm/>"
							                 , "-H"             ,   "-20"
							                 , "-i"             ,   "<icon=ac10.xbm/>"
		 				                   , "-o"             ,   "<icon=batt10.xbm/>" 
		 				                   ] 10
        
        -- wireless monitor
	      , Run Wireless         "wlp2s0" 
		                           [ "-a"             ,   "l"
						                   , "-w"             ,   "4"
						                   , "-t"             ,   "<icon=wifi_01.xbm/><quality>%"
                               , "-h"             ,   "#c0c0c0"
						                   ] 10
        
        -- log monitor
        , Run XMonadLog

        -- pipereader launcher
        , Run PipeReader       "/tmp/pipe"            "Update"        

        -- cpu usage popup launcher
        , Run Com              "TCPU"             []  ""            10

        -- traffic usage popup launcher
        , Run Com              "XMTraf"           []  ""            50
		    
        -- volume launcher
        , Run Com              "XMVol"            []  "myVolume"    10

        -- kernel monitor
        , Run Com              "uname"  ["-s","-r"]   ""            36000

	      -- updates monitor
        , Run Com              "XMUpdate"         []  ""            3600
        
        -- time and date popuo launcher 
        , Run Com              "XMTime"           []  ""            10
        ]
        -- layout
   , sepChar  =  "%"   -- delineator between plugin names and straight text
   , alignSep =  "}{"  -- separator between left-right alignment
   , template =  " <fc=#3aa4db>|</fc> %XMonadLog% <fc=#3aa4db>|</fc> %coretemp% <fc=#3aa4db>|</fc><action=`XMFree` button=1> %memory% </action><fc=#3aa4db>|</fc><action=`XMVnstat` button=1> %XMTraf% </action><fc=#3aa4db>|</fc><action=`XMTop-cpu` button=1> %multicpu% %TCPU%</action><fc=#3aa4db>|</fc>}<action=`XMCal` button=1> %XMTime% </action>{<action=`XMYaourt` button=1>%Update%%XMUpdate%</action> <fc=#3aa4db>|</fc> <icon=cpu5.xbm/> %uname% <fc=#3aa4db>|</fc> <fc=#c0c0c0><icon=vol2.xbm/> %myVolume% </fc><fc=#3aa4db>|</fc> %wlp2s0wi% <fc=#3aa4db>|</fc> %battery% <fc=#3aa4db>|</fc> "
      
   }

