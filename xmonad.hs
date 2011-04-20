import XMonad hiding ( (|||) ) -- don't use the normal ||| operator
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
-- Urgency messages
import Data.List
import XMonad.Hooks.UrgencyHook
import System.IO

import XMonad.Layout.LayoutCombinators -- ise the one from LayoutCombinators instead
import XMonad.Layout.Named
import XMonad.Layout.DwmStyle
-- layouts
import XMonad.Layout.NoBorders -- (smartBorders)
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.IM
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Grid

-- Data.Ratio for IM layout
import Data.Ratio ((%))

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName


myWorkspaces = ["1:web","2:im","3:files","4:term","5:emacs","6:office","7:media"] ++ map show [8..9]

myManageHook = composeAll . concat $
    [ [ className =? "Gimp"          --> doFloat ]
    , [ className =? "Vncviewer"     --> doFloat ]
    , [ appName   =? "xmessage"      --> doFloat ]
    , [ appName   =? "stardict"      --> doFloat ]
    , [ className =? "Vlc"           --> doFloat ]
    , [ className =? "Vlc"           --> moveTo "7:media" ]
    , [ className =? "Gajim.py"      --> moveTo "2:im" ]
    , [ className =? "psi"           --> moveTo "2:im" ]
    , [ className =? "Pidgin"        --> moveTo "2:im" ]
    , [ className =? "Google-chrome" --> moveTo "1:web" ]
    , [ className =? "Emacs"         --> moveTo "5:emacs" ]
    , [ className =? "Firefox"       --> moveTo "1:web" ]
    , [ appName   =? "xterm"         --> moveTo "4:term" ]
    , [ appName   =? "urxvt"         --> moveTo "4:term" ]
    , [ className =? "OpenOffice.org 3.2" --> moveTo "6:office" ]
    , [ className =? "Xpdf"          --> moveTo "6:office" ]
    , [ className =? c --> doFloat | c <- myFloatsC ]
    , [ fmap ( c `isInfixOf`) className --> doFloat | c <- myMatchAnywhereFloatsC ]
    , [ fmap ( c `isInfixOf`) title --> doFloat | c <- myMatchAnywhereFloatsT ]
    ]
      where moveTo = doF . W.shift
            myFloatsC = ["Gajim.py", "Xmessage", "feh", "MPlayer"]
            myMatchAnywhereFloatsC = ["Google"]
            myMatchAnywhereFloatsT = ["Save a Bookmark"] -- this one is silly for only one string! 

myKeys =
    [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
    , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
    , ((0, xK_Print), spawn "scrot")
    , ((mod4Mask, xK_e), spawn "emacsclient -cna ''")
    ]

--- My Theme For Tabbed layout
myTheme = defaultTheme { decoHeight = 16
                        , activeColor = "#a6c292"
                        , activeBorderColor = "#a6c292"
                        , activeTextColor = "#000000"
                        , inactiveBorderColor = "#000000"
                        }

--LayoutHook
myLayouts  =  onWorkspace "2:im" imLayout $  onWorkspace "1:web" webL $  onWorkspace "9" gimpL $ onWorkspace "5:emacs" fullL $ onWorkspace "6:office" officeL $ onWorkspace "7:media" fullL $ onWorkspace "8" fullL $ standardLayouts 
    where
        standardLayouts =   avoidStruts  $ (tiled ||| reflectTiled ||| Mirror tiled ||| Grid ||| Full) 
 
        --Layouts
        tiled = smartBorders (ResizableTall 1 (2/100) (1/2) [])
        reflectTiled = (reflectHoriz tiled)
        tabLayout = (tabbed shrinkText myTheme)
        full = noBorders Full
 
        --Im Layout
        imLayout = avoidStruts $ smartBorders $ withIM ratio psiRoster $ reflectHoriz $ withIM ratio pidginRoster $ reflectHoriz $ withIM skypeRatio skypeRoster (tiled ||| reflectTiled ||| Grid) 
            where
                chatLayout      = Grid
                ratio = (1%9)
                skypeRatio = (1%8)
                pidginRoster    = And (ClassName "Pidgin") (Role "buddy_list")
                psiRoster       = And (Resource "main") (ClassName "psi")
                skypeRoster     = (ClassName "Skype") `And` (Not (Title "Options")) `And` (Not (Role "Chats")) `And` (Not (Role "CallWindowForm"))

        --Gimp Layout
        gimpL = avoidStruts $ smartBorders $ withIM (0.11) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.15) (Role "gimp-dock") Full 
 
        --Web Layout
        webL      = avoidStruts $  tabLayout  ||| tiled ||| reflectHoriz tiled |||  full 
        
        --Office Layout
        officeL = avoidStruts $  tabLayout  ||| tiled ||| reflectHoriz tiled |||  full
 
        --VirtualLayout
        fullL = avoidStruts $ tabLayout  ||| tiled ||| reflectHoriz tiled |||  full


-- myTerminal = "xterm -name scratchpad -e screen -q -S rgr -d -R"
-- myTerminal = "uxterm"
myTerminal = "urxvt"

myFont = "-xos4-terminus-medium-r-normal--12-120-72-72-c-60-*-*"
-- myFont = "xft:Terminus:size=12"

-- real Goerzen config style
main = do
        xmproc <- spawnPipe "/usr/local/bin/xmobar /home/saganov/.xmobarrc"
        xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
                   { manageHook = manageDocks <+> myManageHook -- make sure to include myManageHook definition from above
                        <+> manageHook defaultConfig
                   , workspaces = myWorkspaces
                   , terminal =  myTerminal
                   , layoutHook = myLayouts --avoidStruts  $  layoutHook defaultConfig
                   , startupHook = ewmhDesktopsStartup >> setWMName "LG3D"
                   , logHook = dynamicLogWithPP $ xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppCurrent  = xmobarColor "yellow" "" . wrap "[" "]"
                        , ppVisible = wrap "(" ")"
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        , ppUrgent = xmobarColor "black" "red" . xmobarStrip
                        }
                        , modMask = mod4Mask -- Use Super instead of Alt
                } `additionalKeys` myKeys
