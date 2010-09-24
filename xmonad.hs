-- -*- mode: haskell; mode: rainbow -*-
import XMonad hiding ((|||))
import qualified XMonad.StackSet as W

import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run
 
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.SinkAll
import XMonad.Actions.WindowGo
import XMonad.Actions.SpawnOn

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Grid (Grid(..))
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.LayoutCombinators (JumpToLayout(..), (|||))
import XMonad.Layout.TwoPane (TwoPane(..))
import XMonad.Layout.ThreeColumns (ThreeCol(..))
import XMonad.Layout.Named (named)
import XMonad.Layout.ResizableTile

import XMonad.Prompt
import XMonad.Prompt.Shell ( shellPrompt )
import XMonad.Prompt.Man ( manPrompt )
import XMonad.Prompt.Window

import Data.List (isPrefixOf)

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ (defaultConfig {
    terminal = myTerm
    , focusFollowsMouse = True
    , borderWidth = 1
    , normalBorderColor = "#000000"
    , focusedBorderColor = "#9A0000"
    , workspaces = ["comm", "browse", "code", "mail", "view"] ++ map show [6..9]
    , modMask = mod4Mask -- use the Windows button as mod
    , layoutHook = myLayout
    , logHook = dynamicLogWithPP $ xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
    , manageHook = myManageHook
    , startupHook = myStartupHook
    }
            `additionalKeysP`
            [ ("M-<Backspace>", restart "xmonad" True)
            , ("M-S-<Backspace>", spawn quitKDE)
              -- Prompts/Launcher
            , ("M-p", shellPrompt promptConfig)
            , ("M-S-p", spawn "krunner")
            , ("M-y", spawn launcher)
            , ("M-S-y", spawn termLauncher)
            , ("M-g", windowPromptGoto promptConfig)
            , ("M-z", manPrompt promptConfig)
            , ("M-b", windowPromptBring promptConfig)
            , ("M-S-b", windowPromptBringCopy promptConfig)
              -- Window/workspace management
            , ("M-S-h", sendMessage MirrorShrink)
            , ("M-S-l", sendMessage MirrorExpand)
            , ("M-<Escape>", kill)
            , ("M-u", focusUrgent)
            , ("M-S-t", sinkAll)
            , ("M-s", sendMessage ToggleStruts)
            , ("M-<Tab>", nextWS)
            , ("M-S-<Tab>", prevWS)
            , ("M-C-<Tab>", toggleWS)
            , ("M-<R>", nextWS)
            , ("M-<L>", prevWS)
            , ("M-c", windows copyToAll)
            , ("M-S-c", killAllOtherCopies)
              -- Apps
            , ("M-f", runOrRaise "firefox" (className =? "Firefox"))
            , ("M-S-f", raiseMaybe (runInTerm "" "newsbeuter") (title =? "newsbeuter"))
            , ("M-i", raiseMaybe (runInTerm "" "weechat-curses") (fmap ("weechat" `isPrefixOf`) title))
              -- Layoutjumper
            , ("M-<F2>", sendMessage $ JumpToLayout "Two")
            , ("M-<F3>", sendMessage $ JumpToLayout "Three")
            , ("M-<F12>", sendMessage $ JumpToLayout "Full")
            ])
    where
      myStartupHook = ewmhDesktopsStartup >> setWMName "LG3D"
      quitKDE = "dbus-send --print-reply --dest=org.kde.ksmserver /KSMServer org.kde.KSMServerInterface.logout int32:1 int32:0 int32:1"
      myTerm = "urxvtcd"
      termExec = myTerm ++ " -e"
      dmenuOptions = buildOptions [ ("-fn", promptFont)
                                  , ("-nb", promptBG)
                                  , ("-sb", promptBG)
                                  , ("-nf", promptNFG)
                                  , ("-sf", promptSFG)
                                  ]
        where buildOptions = concat . map (\(flag, value) -> " " ++ flag ++ " '" ++ value ++ "'")
      launcher = "cmd=$(yeganesh -- -p 'Run:'" ++ dmenuOptions ++ ") && $cmd"
      termLauncher = "cmd=$(yeganesh -p withTerm -- -p 'Run in Terminal:'" ++ dmenuOptions ++ ") && " ++ termExec ++ " $cmd"

promptFont = "xft:inconsolata:size=14:antialias=true:hinting=true:hintstyle=hintfull"
promptBG = "#171717"
promptNFG = "#ff7701"           -- non-selected Foreground
promptSFG = "#00aa4a"           -- selected Foreground
promptConfig = defaultXPConfig { font = promptFont
                             , bgColor = promptBG
                             , fgColor = promptNFG
                             , bgHLight = promptSFG
                             , fgHLight = promptBG
                             , promptBorderWidth = 0
                             , height = 16
                             , historySize = 512
                             , historyFilter = deleteConsecutive
                             }

-- Layouts ------------------------------------
twoPane = named "Two" $ TwoPane 0.04 0.5
threePane = named "Three" $ ThreeCol 1 0.04 0.4

myLayout = smartBorders $ avoidStruts (
  onWorkspace "comm" (unevenTile ||| Grid ||| Full) $
  tiled ||| Mirror tiled ||| twoPane ||| threePane ||| Full)
  where
    unevenTile = ResizableTall 2 incDelta 0.8 []
    tiled = ResizableTall 1 incDelta goldenRatio []
    goldenRatio = toRational (2/(1 + sqrt 5 :: Double))
    incDelta = 0.04
----------------------------------------

myManageHook = (composeAll . concat $
               [ [ className =? f --> doFloat          | f <- floats ]
                ,[ className =? b --> doShift "browse" | b <- browse ]
                ,[ className =? c --> doShift "code"   | c <- code ]
                ,[ className =? c --> doShift "comm"   | c <- comms ]
                ,[ className =? i --> doIgnore         | i <- ignores ]
                ,[ isFullscreen --> doFullFloat
                 , isDialog     --> doCenterFloat
                 , title =? "Wanderlust Mail" --> doShift "mail"
                 , title =? "newsbeuter" --> doShift "comm"
                 , fmap ("weechat" `isPrefixOf`) title --> doShift "comm"
                 ] ])
               <+> manageDocks
  where ignores = []
        floats = ["MPlayer", "Smplayer", "Plasma-desktop", "Lancelot"]
        browse = []
        code  = []
        comms = ["Kopete"]

--  Local Variables:
--  compile-command: "xmonad --recompile"
--  End:
