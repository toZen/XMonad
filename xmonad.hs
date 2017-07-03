-- XMonad configuration file

-- Core
import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.RunOrRaise
import XMonad hiding ( (|||) )
import Control.Monad (liftM2)
import Data.Ratio ((%))
import Foreign.C (CChar)
import System.Exit
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified Data.ByteString as B

-- Hooks
import XMonad.Hooks.Minimize
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Place
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.FloatNext (floatNextHook, toggleFloatNext, toggleFloatAllNew)
import XMonad.Hooks.WorkspaceByPos
import XMonad.Hooks.SetWMName

-- Util
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (safeSpawn, unsafeSpawn, runInTerm, spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig

-- Actions
import XMonad.Actions.Promote
import XMonad.Actions.FloatSnap
import XMonad.Actions.FloatKeys
import XMonad.Actions.UpdateFocus
import XMonad.Actions.WorkspaceNames

-- Layouts
import XMonad.Layout.Minimize
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.LayoutCombinators ((|||))
import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.NoBorders
import XMonad.Layout.Master
import XMonad.Layout.Minimize
import XMonad.Layout.FixedColumn
import XMonad.Layout.WindowNavigation
import XMonad.Layout.PerWorkspace
import qualified XMonad.Layout.ToggleLayouts as Tog

-- Options
myFM                  =  "urxvtc -name ranger -e ranger"
myHtop                =  "urxvtc -name htop -e htop"
myTerminal            = "urxvtc"
terminalClass         = "URxvt"
myShell               = "bash"
myFocusFollowsMouse   = False
myClickJustFocuses    = False
myBorderWidth         = 1
myModMask             = mod4Mask
myWorkspaces          = [ "WEB", "CODE", "TERM", "MAIL", "FILES"]
myNormalBorderColor   = "#000000"
myFocusedBorderColor  = "#51c4d4"
myBackgroundColor     = "#fe8e38"
myForegroundColor     = "#c7c9cb"
role                  = stringProperty "WM_WINDOW_ROLE"
encodeCChar           = map fromIntegral . B.unpack
scratchPad            = scratchpadSpawnActionTerminal "urxvtc -name scratchpad"

-- MyKeyBindings
myKeys conf@(XConfig    {XMonad.modMask = modm})    =    M.fromList $
    [ 
      ((mod1Mask,                         0xff61 ), spawn         "scrot  -s -e 'mv $f ~/Pictures/Screenshots/ 2>/dev/null'") --PrtSc+Alt
    , ((0,                                0xffc9 ), scratchPad                                                              ) --F12
    , ((0,                                0xff61 ), spawn         "scrot  -e 'mv $f ~/Pictures/Screenshots/ 2>/dev/null'"   ) --PrtSc
    , ((0,                                0xffbe ), namedScratchpadAction mynameScratchpads   "MyFM") --F1
    , ((0,                                0xffbf ), namedScratchpadAction mynameScratchpads "MyHtop") --F2
    , ((0,                            0x1008ff13 ), spawn         "amixer -q set Master        2dB+") --Fn+F12
    , ((0,                            0x1008ff11 ), spawn         "amixer -q set Master        2dB-") --Fn+F11
    , ((0,                            0x1008ff12 ), spawn         "amixer -q set Master  toggleMute") --Fn+F10
    , ((modm,                               xK_a ), spawn         "urxvtc -name  ncmpcpp -e ncmpcpp") --Win+A
    , ((modm         .|. shiftMask,         xK_b ), spawn         "bleachbit"                       ) --Shift+Win+B
    , ((modm,                               xK_d ), spawn         "sdmenu"                          ) --Win+D
    , ((modm,                               xK_e ), spawn         "thunderbird"                     ) --Win+E
    , ((modm,                               xK_f ), spawn         "pcmanfm"                         ) --Win+F
    , ((modm,                               xK_g ), spawn         "gksu gparted"                    ) --Win+G
    , ((modm,                               xK_l ), spawn         "lxappearance"                    ) --Win+L
    , ((modm,                               xK_p ), spawn         "dmenu_run"                       ) --Win+P
    , ((modm         .|. shiftMask,         xK_g ), spawn         "gmrun"                           ) --Shift+Win+G
    , ((modm,                               xK_q ), spawn         "xmonad --recompile && xmonad --restart && killall xmobar && xmobar") --Win+Q
    , ((modm,                               xK_s ), spawn         "subl3"                           ) --Win+S
    , ((modm,                               xK_w ), spawn         "vivaldi-snapshot"                ) --Win+W
    , ((modm         .|. shiftMask,         xK_x ), spawn         "oblogout"                        ) --Shift+Win+X
    , ((modm,                               xK_x ), spawn         "subl3 ~/.xmonad/xmonad.hs"       ) --Win+X
    , ((modm         .|. shiftMask,         xK_z ), spawn         "XMYaourt"                        ) --Shift+Win+Z
    , ((modm,                               xK_c ), manPrompt                                    def)
    , ((modm         .|. controlMask,       xK_x ), runOrRaisePrompt                             def)
    , ((modm,                               xK_m ), withFocused minimizeWindow                      )
    , ((modm,                               xK_n ), sendMessage RestoreNextMinimizedWin             )

-- Default KeyBindings 
    , ((modm         .|. shiftMask,         xK_c ), kill                                            )
--  , ((modm,                               xK_d ), sendMessage   Expand                            )
--  , ((modm,                               xK_g ), sendMessage   Shrink                            )
    , ((modm,                               xK_j ), windows       W.focusDown                       )
    , ((modm         .|. shiftMask,         xK_j ), windows       W.swapDown                        )
    , ((modm,                               xK_k ), windows       W.focusUp                         )
    , ((modm         .|. shiftMask,         xK_k ), windows       W.swapUp                          )
    , ((modm         .|. shiftMask,         xK_m ), windows       W.focusMaster                     )
    , ((modm         .|. shiftMask,         xK_n ), refresh                                         )
    , ((modm         .|. shiftMask,         xK_q ), io            (exitWith ExitSuccess)            )
    , ((modm         .|. shiftMask,         xK_t ), withFocused   $ windows . W.sink                )
    , ((modm,                               xK_t ), spawn         $ XMonad.terminal conf            )
    , ((modm,                             xK_Tab ), windows       W.focusDown                       )
    , ((modm,                           xK_space ), sendMessage   NextLayout                        )
    , ((modm         .|. shiftMask,     xK_space ), setLayout     $ XMonad.layoutHook conf          )
    , ((modm,                           xK_comma ), sendMessage   (IncMasterN 1)                    )
    , ((modm,                          xK_Return ), windows       W.swapMaster                      )
    , ((modm,                          xK_period ), sendMessage   (IncMasterN (-1))                 )
    ]

    ++
    [ ((m            .|.                 modm, k ), windows       $ f i                             )
    | (i, k) <- zip (XMonad.workspaces      conf )  [xK_1 .. xK_9]
    , (f, m) <- [(W.greedyView,                0 )
    , (W.shift,                        shiftMask )  ]] 
    ++
    [((m             .|.               modm, key ), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_u, xK_i]          [0..]
    , (f, m) <- [(W.view,                      0 ), 
      (W.shift,                      shiftMask)]]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

-- Window rules:
myLayoutHook          =  avoidStruts
          $ minimize
          $ Tog.toggleLayouts (noBorders Full) 
          $ smartBorders
          $ onWorkspace  "WEB"     (Full  ||| Mirror   tiled)
          $ onWorkspace  "CODE"    (Full  ||| Mirror   tiled) 
          $ onWorkspace  "FILES"   (Full  ||| Mirror   tiled)
          $ onWorkspace  "MAIL"    (Full  ||| Mirror   tiled)
          $ onWorkspace  "TERM"    (tiled ||| Full ||| Grid )
          $ tiled ||| Mirror tiled ||| Full
          where
              tiled   = Tall nmaster delta ratio
              nmaster = 1
              ratio   = 1/2
              delta   = 3/100

myManageHook          =  composeAll . concat $
    [
      [className      =? c           --> doF (W.shift "WEB"  )   | c <- myW]
    , [className      =? c           --> doF (W.shift "MAIL" )   | c <- myM]
    , [className      =? c           --> doF (W.shift "FILES")   | c <- myF]
    , [className      =? c           --> doF (W.shift "TERM" )   | c <- myT]
    , [className      =? c           --> doF (W.shift "CODE" )   | c <- myC]

    , [className      =? "mpv"       --> doFullFloat]
    , [className      =? c           --> doCenterFloat       | c <- myFloatC]
    , [appName        =? a           --> doCenterFloat       | a <- myFloatA]
    , [title          =? t           --> doCenterFloat       | t <- myFloatT]
    , [role           =? r           --> doCenterFloat       | r <- myFloatR]
    , [isFullscreen                  --> doFloat]
    , [isDialog                      --> doCenterFloat]
    , [transience']
    , [manageDocks]

    ]
    where
    myW               = ["Vivaldi-snapshot", "Firefox"]
    myM               = ["Thunderbird"]
    myF               = ["Pcmanfm"]
    myT               = ["URxvt"]
    myC               = ["Subl3", "vim"]

-- CenterFloat
    myFloatC          = ["Xmessage","feh"]
    myFloatA          = ["Update","oblogout"]
    myFloatT          = ["Software Update"]
    myFloatR          = ["task_dialog","messages","pop-up","^conversation$","About"]


-- Event handling
myEventHook           =  handleEventHook def <+> fullscreenEventHook <+> docksEventHook <+> ewmhDesktopsEventHook 

-- Status bars and logging
myLogHook             = do
            dynamicLogString $ xmobarPP {
            ppCurrent = xmobarColor "#ff6500" ""
          , ppTitle   = (\str -> "") 
          , ppLayout  = (\str -> "")
                      }

-- Startup hook
myStartupHook         =  return () <+> adjustEventInput <+> setWMName "XMonad"

-- nameScratchpad
mynameScratchpads     = [ NS "update" "urxvtc -name update -e yaourt -Syua" (appName  =? "update") (customFloating $ W.RationalRect 0.31 0.3 0.4 0.3)
                        , NS "MyHtop"       myHtop           (appName    =? "htop")         (customFloating $ W.RationalRect 0.05 0.1 0.9 0.8)
                        , NS "MyFM"         myFM             (appName    =? "ranger")       (customFloating $ W.RationalRect 0.15 0.2 0.7 0.6)
    ]

-- Scratchpad
manageScratchPad      = scratchpadManageHook (W.RationalRect l t w h)
    where
    h                 = 0.333   -- terminal height
    w                 = 1       -- terminal width
    t                 = 1 - h   -- distance from top edge
    l                 = 1 - w   -- distance from left edge

main = do
       xmonad         =<< xmobar myConfig 

myConfig              = ewmh $ def {
   terminal           = myTerminal
 , focusFollowsMouse  = myFocusFollowsMouse
 , clickJustFocuses   = myClickJustFocuses
 , borderWidth        = myBorderWidth
 , modMask            = myModMask
 , workspaces         = myWorkspaces
 , normalBorderColor  = myNormalBorderColor
 , focusedBorderColor = myFocusedBorderColor
 , keys               = myKeys
 , mouseBindings      = myMouseBindings
 , layoutHook         = myLayoutHook
 , manageHook         = manageHook def <+> myManageHook <+> manageScratchPad <+> namedScratchpadManageHook  mynameScratchpads <+> placeHook (smart (0.5,0.5)) <+> workspaceByPos
 , handleEventHook    = myEventHook
 , logHook            = myLogHook >>= xmonadPropLog
 , startupHook        = myStartupHook
    }


