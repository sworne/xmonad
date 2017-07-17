{-# LANGUAGE DeriveGeneric #-}

-- IMPORTS

    -- Base
import XMonad
import Data.Maybe (isJust)
import Data.List
import XMonad.Config.Azerty
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import  qualified XMonad.StackSet as W

    -- Utilities
import XMonad.Util.EZConfig (additionalKeysP, additionalMouseBindings)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (safeSpawn, unsafeSpawn, runInTerm, spawnPipe)
import XMonad.Util.SpawnOnce

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, defaultPP, dzenColor, pad, shorten, wrap, PP(..))
import XMonad.Hooks.ManageDocks (avoidStruts, ToggleStruts(..))
import XMonad.Hooks.Place (placeHook, withGaps, smart)
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.FloatNext (floatNextHook, toggleFloatNext, toggleFloatAllNew)

    -- Actions
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.CopyWindow (kill1, copyToAll, killAllOtherCopies, runOrCopy)
import XMonad.Actions.WindowGo (runOrRaise, raiseMaybe)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..))
import XMonad.Actions.GridSelect (GSConfig(..), goToSelected, bringSelected, colorRangeFromClassName, buildDefaultGSConfig)
import XMonad.Actions.DynamicWorkspaces (addWorkspacePrompt, removeEmptyWorkspace)
import XMonad.Actions.UpdatePointer
import qualified XMonad.Actions.ConstrainedResize as Sqr

    -- Layouts modifiers
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Renamed (renamed, Rename(CutWordsLeft, Replace))
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.Minimize
import XMonad.Layout.Maximize
import XMonad.Layout.BoringWindows (boringWindows)
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.Reflect (reflectVert, reflectHoriz, REFLECTX(..), REFLECTY(..))
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), Toggle(..), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
-- import XMonad.StackSet as W

    -- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.OneBig
import XMonad.Layout.ZoomRow (zoomRow, zoomIn, zoomOut, zoomReset, ZoomMessage(ZoomFullToggle))
import XMonad.Layout.IM (withIM, Property(Role))
import XMonad.Layout.NoBorders

    -- Prompts
import XMonad.Prompt (defaultXPConfig, XPConfig(..), XPPosition(Top), Direction1D(..))

    -- YAML
import GHC.Generics
import Data.Yaml

   -- Padding
import Padding
                                  --------------------



    -- Styles
myFont          = "terminus"
myBorderWidth   = 6
myColorBG       = "#1f1f1f"
myColorWhite    = "#c0b18b"
myColorRed      = "#c0b18b"
myColorBrown    = "#c0b18b"

    -- Settings
myModMask       = mod4Mask
myTerminal      = "urxvt"
myMusic         = "spotify"



myKeys = 
    -- XMonad
    [ ("M-M1-q", io exitSuccess)

    -- Windows
    , ("M-q",               kill1)
    , ("M-x",               windows W.swapDown)
    , ("M-z",               windows W.swapUp)
    , ("M-<Tab>",               windows W.focusDown)
    , ("M-a" ,             sendMessage NextLayout)

    -- windows hacks
        , ("M-<Right>",               sendMessage Shrink)
        , ("M-<Left>",               sendMessage Expand)
        , ("M-<Down>;",             sendMessage zoomReset)
        , ("M-<Up>",               sendMessage ZoomFullToggle)

    -- Apps
        , ("M-<Return>",        spawn "urxvt")
        , ("M-S-<Return>",      spawn "google-chrome-stable")
        , ("M-<Space>",         spawn "rofi -show run")
        , ("M-l",               spawn "lockscr")
    -- Scratchpads
        , ("M-<h>",          namedScratchpadAction scratchpads "music")
        --, ("M-<[>",           scratchMusic)
        --, ("M-<]>",           namedScratchpadAction myScratchpads "music")

    ] 


    -- workspaces
myWorkspaces = ["one", "two"]

scratchpads = [
    --NS "term" "gnome-terminal --hide-menubar --role=scratchpad" (role =? "scratchpad") (customFloating $ W.RationalRect (1/12) (1/12) (5/6) (5/6)),
    --NS "irc" "gnome-terminal --role=irc" (role =? "irc") (customFloating $ W.RationalRect (1/12) (1/12) (5/6) (5/6)),
    -- NS "applaunch" "xfce4-appfinder -c" (title =? "Application Finder") defaultFloating ,
    NS "music" "spotify" (role =? "music") (customFloating $ W.RationalRect (1/2) (1/2) (1/2) (1/2))]
        where role = stringProperty "WM_WINDOW_ROLE"


myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Rofi"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore ] <+>  namedScratchpadManageHook scratchpads


dsp = "xrandr  "

myStartupHook = do
          spawnOnce "unclutter &"
          spawnOnce "compton &"
          spawnOnce "~/bin/newbg &"
          spawnOnce dsp

myLayoutHook =  smartBorders$ (vertical ||| centered ||| horizontal ||| grid)
    where 
        grid = padding 20 20 $  Grid (4/3)
        centered = padding 0 0 $ Full
        vertical = padding 40 40 $ Tall 3 (5/100) (50/100)
        horizontal = padding 20 20 $  Tall  1 (6/10) (4/10)
       
      
main = do
    xmonad       $  azertyConfig
        { modMask            = myModMask
           , terminal           = myTerminal
           , manageHook         = myManageHook 
           , layoutHook         = myLayoutHook 
           , startupHook        = myStartupHook
           , workspaces         = myWorkspaces
           , borderWidth        = myBorderWidth 
           , normalBorderColor  = myColorBG
           , focusedBorderColor = myColorWhite
        } `additionalKeysP`         myKeys 

