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

    -- Prompts
import XMonad.Prompt (defaultXPConfig, XPConfig(..), XPPosition(Top), Direction1D(..))

    -- YAML
import GHC.Generics
--import Data.Yaml

-- Theme
myFont = "inconsolata"
myBorderWidth   = 5
myColorBG       = "#1f1f1f"
myColorWhite    = "#ffc500"
myColorRed      = "#ff7322"
myColorBrown    = "#c0b18b"
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
myBG            = "hsetroot -solid '" ++ myColorBG ++ "' &"
myCompositor    = "ps aux |grep '[c]ompton' ||compton &"
myNotes         =  myBrowser ++ " --app='https://keep.google.com/'"
myBar           =  "./.cabal/bin/xmobar ~/.xmonad/xmobar.hs"


bar = gaps [(U,40), (D,10), (L,0), (R,0)] $ smartBorders $ tabbed shrinkText myTabConfig
two = Tall 1 (1/50) (1/4)
clss = ClassName "Spotify" `Or` Role "pop-up"
data BAR = BAR deriving (Read, Show, Eq, Typeable)
instance Transformer BAR Window
    where
      transform _ x k = k (combineTwoP two bar x clss) (const x)

mini f z [] = z
mini f z (x : xs) = x `f` mini f z xs



-- For key codes see:
-- http://hackage.haskell.org/package/xmonad-contrib-0.14/docs/XMonad-Util-EZConfig.html
myKeys =
    -- XMonad
    [ ("M-M1-q", io exitSuccess)

    -- Windows
    , ("M-q",            kill1)
    , ("M-z",            windows W.swapUp)
    , ("M-<Tab>",        windows W.focusDown)
    , ("M-a" ,           sendMessage NextLayout)
    , ("M-x",            sendMessage Shrink)
    , ("M-s",            sendMessage Expand)
    , ("M-w",            withFocused $ windows . W.sink)
    , ("M-,",            prevWS)
    , ("M-.",            nextWS)

    -- Apps
    , ("M-<Return>",     spawn myTerminal)
    , ("M-S-<Return>",   spawn myBrowser)
    , ("M-<Space>",      spawn myLauncher)
    , ("M-l",            spawn myLock)

    -- Toggle
    --, ("M-<Page_Up>",    map (hideWindow) (allWithProperty clss))
    --, ("M-<Page_Up>",    map mini (allWithProperty clss) hideWindow)
    , ("M-<Delete>",     sendMessage $ Toggle BAR)
    , ("M-f",            sendMessage $ Toggle NBFULL)
    ]

-- workspaces
myWorkspaces = ["-", "--"]

scratchpads = [ NS "notes" spawnNotes findNotes manageNotes]

-- Notepad
  where
    spawnNotes  = myNotes
    findNotes   = resource =? "notes"
    manageNotes = nonFloating

-- Hooks
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Rofi"           --> doFloat
    ] <+>  namedScratchpadManageHook scratchpads

myStartupHook = do
    spawn myCompositor
    spawn myBG
    spawn myBar
    spawn "unclutter &"


-- Layouts
myLayoutHook =  mkToggle (single BAR) $ mkToggle (single NBFULL) (
                hiddenWindows $
                gapS grid |||
                gapS gold |||
                gapS vertical |||
                gapS tabs |||
                gapL tabs)
    where
        full = smartBorders Full
        grid = padS $ Grid (4/4)
        vertical = padS $ Tall 3 (5/100) (50/100)
        tabs = tabbed shrinkText myTabConfig
        two = Tall 1 (1/50) (1/4)
        drawer = simpleDrawer 0.001 0.001 clss
        gold   = Tall 1 0.03 ratio
        ratio = toRational (2/(1 + sqrt 5 :: Double)) -- golden ratio

        gapS =  gaps [(U,40), (D,10), (L,20), (R,20)]
        gapM = gaps [(U,60), (D,60), (L,30), (R,30)]
        gapL = gaps [(U,100), (D,100), (L,100), (R,100)]
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
