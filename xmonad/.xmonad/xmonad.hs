{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleContexts #-}

-- XMonad Core Imports
import XMonad
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

-- Standard
import Control.Lens hiding ((??), elements)

-- Config
import XMonad.Config.Azerty

-- Utilities
import XMonad.Util.EZConfig (additionalKeysP, additionalMouseBindings)
import XMonad.Util.Run (safeSpawn, unsafeSpawn, runInTerm, spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.Loggers

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (avoidStruts, ToggleStruts(..))
import XMonad.Hooks.XPropManage
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks

-- Actions
import XMonad.Actions.Promote
import XMonad.Actions.CycleWS (moveTo, shiftTo,prevWS, nextWS, WSType(..))
import XMonad.Actions.GridSelect (GSConfig(..), goToSelected, bringSelected, colorRangeFromClassName, buildDefaultGSConfig)
import XMonad.Actions.CopyWindow hiding (copy)
import XMonad.Actions.TagWindows
import XMonad.Actions.CycleWindows

-- Layouts
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.WindowNavigation as WN
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
import XMonad.Layout.Gaps
import XMonad.Layout.DragPane
import XMonad.Actions.TagWindows
import XMonad.Layout.MultiToggle
import XMonad.Layout.NoBorders
import XMonad.Layout.ComboP
import XMonad.Layout.Combo
import XMonad.Layout.TwoPane
import XMonad.Layout.IfMax
import XMonad.Layout.BoringWindows (boringWindows, focusUp, markBoring)


-- Prompt
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad
import XMonad.Prompt.ConfirmPrompt


-- Local
import Unsplash
import Sidebar

-- Mod key
myModMask       = mod4Mask -- Super

-- Defualt Apps
myBrowser       = "google-chrome-stable"
myTerminal      = "urxvt"
myLock          = "env XSECURELOCK_SAVER=saver_mplayer xsecurelock"
myMusic         = "LD_PRELOAD=/usr/lib/libcurl.so.3:~/.xmonad/spotifywm.so $(which spotify)"
myCompositor    = "compton"
myPWAs          = [
                      ("chat.google.com", "chat.google.com")
                    , ("keep.google.com", "keep.google.com")
                    , ("tasks.google.com/embed/list/~default?fullWidth=1", "tasks.google.com")
                ]
myBar           =  "~/.cabal/bin/xmobar"
myIRC           = "hexchat"

-- Wallpaper
myBgCmd         = "feh --bg-fill /tmp/bg.png"
myKeyFile       = "~/.unsplash-key"

-- dock
myDockApps      = pwaList myBrowser myPWAs ++ [(myMusic, "spotify"), ("hexchat", "hexchat")]
myDockTag       = "dock"
myNormalTag     = "undocked"

-- Misc
myAutostart     = [myCompositor, myBar]
myNext          = "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next"
myPrev          = "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous"
myPlayPause     = "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause"

-- Theme
myFont =  "xft:Inconsolata:size=9"
myBorderWidth   = 6
myColorDark       = "#1f1f1f"
myColorGrey       = "#8e9eab"
myColorLight      = "#f7ece2"
myColorWhite      = "#fdd6b5"

-- workspaces
myWorkspaces = ["⬤", "⬤ ⬤", "⬤ ⬤ ⬤", "⬤ ⬤ ⬤ ⬤"]

-- Keyboard shortcuts (for key codes see):
-- http://hackage.haskell.org/package/xmonad-contrib-0.14/docs/XMonad-Util-EZConfig.html
myKeys =
    [
    -- WM
      ("M-M1-q",         confirmPrompt myPromptConfig "exit" $ io exitSuccess)
    , ("M-l",            spawn myLock)
    
    -- Windows
    , ("M-q",            kill1)
    , ("M-z",            windows W.swapUp)
    , ("M-S-z",          rotUnfocusedUp)
    , ("M-<Tab>",        focusUpTagged myNormalTag)
    , ("M-S-<Tab>",      focusUpTagged myDockTag)
    , ("M-r",            spawnApps myDockApps)
    , ("M-a" ,           sendMessage NextLayout)
    , ("M-x",            sendMessage Shrink)
    , ("M-s",            sendMessage Expand)
    , ("M-w",            withFocused $ windows . W.sink)
    , ("M-[",            nextWS)
    , ("M-]",            prevWS)
    , ("M-S-[",          sendMessage $ WN.Move L)
    , ("M-S-]",          sendMessage $ WN.Move R)
    , ("M-S-`",          withFocused $ toggleTag myDockTag myNormalTag)
    -- http://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Actions-GridSelect.html
    -- http://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Actions-Submap.html
    --, ("M-S-p",        https://hackage.haskell.org/package/xmonad-contrib-0.13/docs/XMonad-Actions-WindowGo.html
    , ("M-,",            spawn myPrev)
    , ("M-.",            spawn myNext)

    -- Apps
    , ("M-<Return>",     spawn myTerminal)
    , ("M-S-<Return>",   spawn myBrowser)
    , ("M-<Space>",      shellPrompt myPromptConfig)
    , ("M-M1-<Space>",   wallPrompt myPromptConfig myUnsplashConfig)
    , ("M-S-<Space>",    windowPrompt myPromptConfig Bring allWindows)

    -- Windows
    , ("M-`",            sequence_ [(sendMessage $ Toggle BAR), copyTagged myDockTag])
    , ("M-h",            sendMessage $ Toggle HIDE)
    , ("M-d",            sendMessage $ Toggle DIFF)
    , ("M-f",            sendMessage $ Toggle NBFULL)
    ] 

-- Tabbed window config
myTabConfig = def {
      activeColor = myColorWhite
    , activeTextColor = myColorDark
    , activeBorderColor = myColorWhite
    , inactiveColor = myColorDark
    , inactiveTextColor = myColorLight
    , inactiveBorderColor = myColorDark
    , fontName = myFont
    , decoHeight = 24
}

myUnsplashConfig = def { queries = ["mountains", "wilderness", "wallpaper"] }

-- Xmobar config
myBarConfig :: PP
myBarConfig = def {   
      ppCurrent          = xmobarColor myColorWhite ""
    , ppVisible          = xmobarColor myColorWhite ""
    , ppHidden           = const ""
    , ppHiddenNoWindows  = const ""
    , ppUrgent           = const ""
    , ppTitle            = const ""
    , ppLayout           = const ""
}

-- Launcher config
myPromptConfig :: XPConfig
myPromptConfig = 
    def { 
      font = myFont
	, fgColor = myColorLight
	, bgColor = myColorDark
	, bgHLight    = myColorDark
    , fgHLight    = myColorWhite
    , promptBorderWidth = 0
    , height = 100
    , position = Bottom
    , historySize = 20
    , showCompletionOnTab = True
    , historyFilter = deleteConsecutive
    }

myGridConfig colorizer = (buildDefaultGSConfig colorizer) {
      gs_font = myFont
    , gs_bordercolor = myColorWhite
    , gs_cellheight = 50
    , gs_cellwidth = 200
}

-- Hooks
myTagHook = myManageHook <+> manageHook def

myManageHook = composeAll
    [ className =? "MPlayer"         --> doFloat
    , className =? "Rofi"            --> doFloat
    , role =? "GtkFileChooserDialog" --> doSink
    , tagWindowGroup dockApps [myDockTag]
    , tagWindowGroup normalApps [myNormalTag]
    ] where 
        role = stringProperty "WM_WINDOW_ROLE"
        doSink = ask >>= doF . W.sink
        dockApps = resourceGroup dockClasses
        normalApps = resourceExcludeGroup dockClasses
        dockClasses = unzip myDockApps ^. _2

-- Layouts
myLayoutHook = let
    l = (gap $ pad $ normal) ||| laptop
    normal = IfMax 6 grid gold
    laptop = IfMax 3 gold tabs 
    grid = GridRatio 1  
    tabs = noBorders $ tabbedBottomAlways shrinkText myTabConfig
    gold = Tall 1 0.03 (toRational (2/(1 + sqrt 5 :: Double)))
    gap =  gaps [(U,20), (D,20), (L,30), (R,30)]
    pad = spacing 10
    tag = Tagged myDockTag
    layout = Tall 1 (1/50) (0/9)
    in
        WN.windowNavigation $
        mkToggle1 BAR $
        mkToggle1 HIDE $
        mkToggle ( NBFULL ?? DIFF ?? EOT ) $ avoidStruts $ combineTwoP layout EmptyLayout l tag

-- Custom Toggled Layouts
data CustomTransformers = BAR | DIFF | FANCY | HIDE
    deriving (Read, Show, Eq, Typeable)

instance Transformer CustomTransformers Window where
    transform BAR x k = let
        layout = Tall 1 (1/50) (2/9)
        dock = boringWindows $ avoidStruts $ noBorders $ tabbedBottomAlways shrinkText myTabConfig
        tag = Tagged myDockTag
        name = renamed [Replace myDockTag]
        in k ( name $ combineTwoP layout dock x tag) (const x)
    transform DIFF x k = let
        layout = dragPane Vertical 0.05 0.5
        diff = avoidStruts $ smartBorders $ tabbedBottomAlways shrinkText myTabConfig
        in k (combineTwo layout diff diff) (const x)
    transform HIDE x k = k (EmptyLayout) (\EmptyLayout -> x)


-- PWA (Progressive web application) creator
pwaThis :: String -> (String, String) -> (String, String)
pwaThis browser (url, clss) = (
    browser 
    ++ " --password-store=gnome --profile-directory=Default --app=https://"
    ++ url
    , clss)

-- Generate list of PWAs from list
pwaList :: String -> [(String, String)] -> [(String, String)]
pwaList browser pwa = map (pwaThis browser) pwa


myStartupHook :: X ()
myStartupHook = do
    setWallpaper myUnsplashConfig
    sequence $ map spawnOnce myAutostart
    spawnApps myDockApps
    spawn "unclutter &"

main = do
    xmproc <- spawnPipe myBar
    xmonad $ docks $ azertyConfig {
      modMask            = myModMask
    , terminal           = myTerminal
    , manageHook         = myTagHook
    , logHook            = dynamicLogWithPP myBarConfig { ppOutput = hPutStrLn xmproc}
    , layoutHook         = myLayoutHook
    , startupHook        = myStartupHook
    , workspaces         = myWorkspaces
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myColorDark
    , focusedBorderColor = myColorWhite
    } `additionalKeysP`    myKeys
