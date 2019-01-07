{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}
-- IMPORTS

    -- Base
import XMonad
import XMonad.Hooks.EwmhDesktops
import Data.Maybe (isJust)
import Data.List
import XMonad.Config.Azerty
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import XMonad.Hooks.ManageHelpers
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
import XMonad.Actions.CopyWindow
import qualified XMonad.Actions.ConstrainedResize as Sqr

    -- Layouts modifiers
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Renamed (renamed, Rename(CutWordsLeft, Replace))
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), Toggle(..), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))

    -- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.NoBorders
import XMonad.Layout.ComboP
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.Column
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CopyWindow(copy)
import XMonad.Layout.BoringWindows
import XMonad.Util.NamedWindows
import XMonad.Util.WindowProperties
import XMonad.Hooks.Minimize
import XMonad.Layout.Hidden
import XMonad.Actions.WithAll
import XMonad.Layout.Drawer
import Numeric (showHex, showIntAtBase)
import XMonad.Actions.TagWindows
import XMonad.Layout.ComboP

    -- Prompts
import XMonad.Prompt (defaultXPConfig, XPConfig(..), XPPosition(Top), Direction1D(..))

    -- YAML
import GHC.Generics
--import Data.Yaml

    -- WALL
import Codec.Picture
import Codec.Picture.Types
import System.IO.Unsafe
import System.Random
import System.Process


-- Theme
myFont = "inconsolata"
myBorderWidth   = 6
myColorBG       = "#1f1f1f"
myColorBG1      = "#8e9eab"
myColorBG2      = "#eef2f3"
myColorWhite    = "#fdd6b5" -- Alt: seedColor
myTabConfig = def {
      activeColor = myColorWhite
    , activeTextColor = myColorBG
    , activeBorderColor = myColorWhite
    , inactiveColor = myColorBG
    , inactiveTextColor = myColorWhite
    , inactiveBorderColor = myColorBG
    , fontName = myFont
}

-- Variables
myModMask       = mod4Mask
myTerminal      = "urxvt"
myMusic         = "LD_PRELOAD=/usr/lib/libcurl.so.3:~/.xmonad/spotifywm.so $(which spotify)"
myBrowser       = "google-chrome-stable"
myLauncher      = "rofi -show run"
myLock          = "env XSECURELOCK_SAVER=saver_mplayer xsecurelock"
myBG            = "/tmp/bg.png"
myBgCmd         = "feh --bg-fill " ++ myBG
myCompositor    = "pkill compton; compton"
myNotes         =  myBrowser ++ " --app='https://keep.google.com/'"
myChat          =  myBrowser ++ " --app='https://chat.google.com/'"
myBar           =  "./.cabal/bin/xmobar ~/.xmonad/xmobar.hs"
myNext          = "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next"
myPrev          = "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous"
-- seedColor       = "#" ++ showHex mySeed "" ++ showHex 24 "" ++ showHex mySeed ""


-- Window Hacks
--dock = combineTwoP hrz bar Full tag
bar = boringWindows (smartBorders $ tabbedBottom shrinkText myTabConfig)
two = Tall 1 (1/50) (2/9)
hrz = Mirror $ Tall 1 (1/50) (1/3)
clss = ClassName "Spotify" `Or` Role "pop-up" `Or` ClassName "Gedit"
--tag = withTagged "dock"
data BAR = BAR deriving (Read, Show, Eq, Typeable)
instance Transformer BAR Window
    where
      transform _ x k = k (combineTwoP two bar x clss) (const x)

data DIFF = DIFF deriving (Read, Show, Eq, Typeable)
instance Transformer DIFF Window
    where
        transform _ x k = k (TwoPane (3/100) (1/2)) (const x)

-- MATH
genSeed :: Int -> Int -> Int
genSeed x y = unsafePerformIO (getStdRandom (randomR (x, y)))

-- WALL
imageCreator :: String -> Int -> IO ()
imageCreator path seed = writePng path $ generateImage pixelRenderer 250 seed
   where pixelRenderer x y = PixelRGB8 (fromIntegral 250) (fromIntegral x) (fromIntegral y)

-- For key codes see:
-- http://hackage.haskell.org/package/xmonad-contrib-0.14/docs/XMonad-Util-EZConfig.html
myKeys =
    [
    -- WM
      ("M-M1-q",         io exitSuccess)
    , ("M-l",            spawn myLock)
    , ("M-p",            sequence_ [liftIO $ imageCreator myBG (genSeed 130 254), spawn myBgCmd])
    , ("M-t",            withFocused (addTag "dock"))

    -- Windows
    , ("M-q",            kill1)
    , ("M-z",            windows W.swapUp)
    , ("M-<Tab>",        windows W.focusDown)
    , ("M-a" ,           sendMessage NextLayout)
    , ("M-x",            sendMessage Shrink)
    , ("M-s",            sendMessage Expand)
    , ("M-w",            withFocused $ windows . W.sink)
    , ("M-,",            spawn myPrev)
    , ("M-.",            spawn myNext)

    -- Apps
    , ("M-<Return>",     spawn myTerminal)
    , ("M-S-<Return>",   spawn myBrowser)
    , ("M-<Space>",      spawn myLauncher)
    , ("M--",            spawn myMusic)
    , ("M-=",            spawn myNotes)
    , ("M-\\",           spawn myChat)

    -- Windows
    , ("M-`",            sendMessage $ Toggle BAR)
    , ("M-d",            sendMessage $ Toggle DIFF)
    , ("M-f",            sendMessage $ Toggle NBFULL)
    ]

-- workspaces
myWorkspaces = ["1", "2"]

-- Hooks
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Rofi"           --> doFloat
    ]

myStartupHook :: X ()
myStartupHook = do
    liftIO $ imageCreator myBG (genSeed 130 254)
    spawn myCompositor
    spawn myBar
    spawn myBgCmd
    spawn "unclutter &"


-- Layouts
myLayoutHook =  avoidStruts $
                mkToggle (single BAR) $
                mkToggle (single NBFULL) $
                mkToggle (single DIFF) (
                    gapS grid |||
                    gapS gold |||
                    gapS vertical |||
                    gapS tabs |||
                    gapL tabs |||
                    gapL grid)
    where
        full = smartBorders Full
        grid = padS $ Grid (4/4)
        vertical = padS $ Tall 3 (5/100) (50/100)
        tabs = tabbed shrinkText myTabConfig
        two = Tall 1 (1/50) (1/4)
        drawer = simpleDrawer 0.001 0.001 clss
        gold   = Tall 1 0.03 ratio
        ratio = toRational (2/(1 + sqrt 5 :: Double))

        gapS =  gaps [(U,20), (D,30), (L,20), (R,20)]
        gapM = gaps [(U,60), (D,60), (L,30), (R,30)]
        gapL = gaps [(U,160), (D,160), (L,160), (R,160)]
        padS = spacing 10
        padL = spacing 20

main = xmonad  $ ewmh  $  azertyConfig
    { modMask            = myModMask
    , terminal           = myTerminal
    , manageHook         = myManageHook
    , layoutHook         = myLayoutHook
    , startupHook        = myStartupHook
    , workspaces         = myWorkspaces
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myColorBG
    , focusedBorderColor = myColorWhite
    } `additionalKeysP`    myKeys
