{-# LANGUAGE DeriveGeneric #-}

-- IMPORTS

    -- Base
import XMonad
import Data.Maybe (isJust)
import Data.List
import XMonad.Config.Azerty
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

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

    -- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.OneBig
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
myChat          = "msg"



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
        , ("M-<Down>",               withFocused $ windows . W.sink)

    -- Apps
        , ("M-<Return>",        spawn "urxvt")
        , ("M-S-<Return>",      spawn "google-chrome-stable")
        , ("M-<Space>",         spawn "rofi -show run")
        , ("M-l",               spawn "lockscr")
    -- Scratchpads
        , ("M-/",           namedScratchpadAction scratchpads "music")
        , ("M-.",           namedScratchpadAction scratchpads "term")
        , ("M-,",           namedScratchpadAction scratchpads "chat")

    ] 


    -- workspaces
myWorkspaces = ["one", "two"]


scratchpads = [ NS "term" spawnTerm findTerm manageTerm
              , NS "chat" spawnChat findChat manageChat
              , NS "music" spawnMusic findMusic manageMusic ]
  where
    spawnMusic  = myMusic ++ " -name music" 
    findMusic   = resource =? "music" -- its window will be named "ncmpcpp" (see above)
    manageMusic = customFloating $ W.RationalRect l t w h -- and I'd like it fixed using the geometry below:
      where
        h = 0.25 -- height, 25%
        w = 1 -- width, 100%
        t = 1 - h -- bottom edge
        l = (1 - w) / 2 -- centered left/right


    spawnChat  = myChat ++ " -name chat" 
    findChat   = resource =? "chat" -- its window will be named "Pavucontrol"
    manageChat = customFloating $ W.RationalRect l t w h -- and I'd like it fixed using the geometry below:
      where
        h = 0.25 -- height, 25%
        w = 1 -- width, 100%
        t = 1 - h -- bottom edge
        l = (1 - w) / 2 -- centered left/right


    spawnTerm  = myTerminal ++ " -name term" -- launch my terminal
    findTerm   = resource =? "term" -- its window will be named "scratchpad" (see above)
    manageTerm = customFloating $ W.RationalRect l t w h -- and I'd like it fixed using the geometry below
      where
        h = 0.25 -- height, 25%
        w = 1 -- width, 100%
        t = 1 - h -- bottom edge
        l = (1 - w) / 2 -- centered left/right


myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Rofi"           --> doFloat
    , className =? "music"        --> doFloat
    , resource  =? "desktop_window" --> doIgnore ] <+>  namedScratchpadManageHook scratchpads


dsp = "xrandr  "

myStartupHook = do
          spawnOnce "unclutter &"
          spawnOnce "compton &"
          spawnOnce "~/bin/newbg &"
          spawnOnce dsp

myLayoutHook =  vertical ||| centered ||| horizontal ||| grid ||| onebig
    where 
        grid = padding 20 20 $  Grid (4/3)
        centered = padding 0 0 $ smartBorders$ Full
        vertical = padding 20 20 $ Tall 3 (5/100) (50/100)
        horizontal = padding 20 20 $  Tall  1 (5/100) (4/10)
        onebig = padding 20 20 $ OneBig (3/4) (3/4)
      
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

