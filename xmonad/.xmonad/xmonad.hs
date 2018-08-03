{-# LANGUAGE DeriveGeneric #-}

-- IMPORTS

    -- Base
import XMonad
import XMonad.Hooks.EwmhDesktops
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
import XMonad.Actions.CycleWS (moveTo, shiftTo,prevWS, nextWS, WSType(..))
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
--import Data.Yaml

   -- Padding
import Padding

    -- Styles
myFont          = "terminus"
myBorderWidth   = 5
myColorBG       = "#1f1f1f"
myColorWhite    = "#ffc500"
myColorRed      = "#ff7322"
myColorBrown    = "#c0b18b"

    -- Settings
myModMask       = mod4Mask
myTerminal      = "urxvt"
myMusic         = "spotify"

myKeys =
    -- XMonad
    [ ("M-M1-q", io exitSuccess)

    -- Windows
    , ("M-q",            kill1)
    , ("M-z",            windows W.swapUp)
    , ("M-<Tab>",        windows W.focusDown)
    , ("M-a" ,           sendMessage NextLayout)

    -- windows hacks
    , ("M-x",            sendMessage Shrink)
    , ("M-s",            sendMessage Expand)
    , ("M-w",            withFocused $ windows . W.sink)
    , ("M-,",            prevWS)
    , ("M-.",            nextWS)
    , ("M-S-,",          prevWS)
    , ("M-S-.",          nextWS)


    -- Apps
    , ("M-<Return>",     spawn myTerminal)
    , ("M-S-<Return>",   spawn "google-chrome-stable")
    , ("M-<Space>",      spawn "rofi -show run")
    , ("M-S-l",          spawn "xsecurelock")

    ]


    -- workspaces
myWorkspaces = ["-", "--", "---" ]

myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Rofi"           --> doFloat]

myStartupHook = do
        spawn "ps aux |grep '[c]ompton' ||compton &"
        spawn "hsetroot -solid \"#1f1f1f\" &"
        spawn "unclutter &"

myLayoutHook =  columns |||
                display |||
                horizontal |||
                grid |||
                centered

    where
        grid = padding 20 20 $ Grid (4/4)
        centered = padding 0 0 $ smartBorders$ Full
        vertical = padding 20 20 $ Tall 3 (5/100) (50/100)
        horizontal = padding 20 20 $  Tall  1 (5/100) (4/10)
        onebig = padding 20 20 $ OneBig (3/4) (3/4)
        columns = padding 30 30 $ Grid (2/2)
        display = padding 100 100 $ Grid (2/2)

main = do
    xmonad  $ ewmh  $  azertyConfig
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
