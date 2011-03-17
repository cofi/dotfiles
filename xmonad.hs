-- -*- mode: haskell; mode: rainbow -*-
import XMonad hiding ((|||))
import qualified XMonad.StackSet as W

import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run
import XMonad.Util.NamedScratchpad
 
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.SinkAll
import XMonad.Actions.WindowGo
import XMonad.Actions.SpawnOn
import qualified XMonad.Actions.Search as S
import XMonad.Actions.Search (SearchEngine(..))
import XMonad.Actions.UpdatePointer

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
import XMonad.Layout.CenteredMaster
import XMonad.Layout.WindowArranger
import XMonad.Layout.SimplestFloat

import XMonad.Prompt
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.DirExec (dirExecPromptNamed)
import XMonad.Prompt.Man (manPrompt)
import XMonad.Prompt.Window

import Data.List (isPrefixOf)
import qualified Data.Map as M
import System.Directory (getHomeDirectory)
import System.Posix.Unistd (getSystemID, SystemID(..))

main = do
--   spawn trayer
  homeDir <- getHomeDirectory
  systemID <- getSystemID
  xmproc <- spawnPipe $ xmobar systemID
  xmonad $ withUrgencyHook NoUrgencyHook $ ewmh $ defaultConfig { terminal = myTerm
                         , focusFollowsMouse = True
                         , borderWidth = 1
                         , normalBorderColor = "#000000"
                         , focusedBorderColor = "#9A0000"
                         , workspaces = ["1:comm", "2:browse", "3:code"] ++ map show [4..9] ++ ["mail", "feeds", "hide"]
                         , modMask = mod4Mask -- use the Windows button as mod
                         , layoutHook = myLayout
                         , logHook = updatePointer (Relative 0.5 0.5) >> (dynamicLogWithPP $ myPP xmproc)
                         , manageHook = myManageHook
                         , startupHook = myStartupHook
                         } `additionalKeysP` myKeys homeDir
    where
      myStartupHook = setWMName "LG3D"
      trayer = "trayer --transparent true --alpha 255 --edge top --align right --padding 2 --expand false " 
               ++ "--heighttype pixel --height 10 --widthtype percent --width 15 --SetPartialStrut true" 
      xmobar systemID = case (nodeName systemID) of
        "hitchhiker" -> "xmobar ~/.xmobar/laptop"
        "coficore"   -> "xmobar ~/.xmobar/desktop"
        _            -> "xmobar ~/.xmobar/default"

myTerm = "urxvtcd"
myKeys homeDir = [ ("M-<Backspace>", spawn respawn)
                 , ("M-S-<Backspace>", spawn logout)
                 , ("M-C-<Backspace>", spawn shutdown)
                   -- Prompts/Launcher
                 , ("M-x", shellPrompt promptConfig)
                 , ("M-S-x", spawn "krunner")
                 , ("M-p", shellPrompt promptConfig)
                 , ("M-S-p", spawn "krunner")
                 , ("M-y", spawn launcher)
                 , ("M-S-y", spawn termLauncher)
                 , ("M-g", windowPromptGoto acPromptConfig)
                 , ("M-b", windowPromptBring acPromptConfig)
                 , ("M-S-b", windowPromptBringCopy acPromptConfig)
                 , ("M-z", manPrompt promptConfig)
                 , ("M-q", dirExecPromptNamed acPromptConfig spawn (withHome ".quick") "Quicks: ")
                 , ("M-S-q", dirExecPromptNamed acPromptConfig spawn (withHome ".quick") "Quicks: ")
                   -- Window/workspace management
                 , ("M-S-h", sendMessage MirrorShrink)
                 , ("M-S-l", sendMessage MirrorExpand)
                 , ("M-u", focusUrgent)
                 , ("M-S-u", clearUrgents)
                 , ("M-S-t", sinkAll)
                   -- movement
                 , ("M-<Tab>", nextNonEmpty)
                 , ("M-S-<Tab>", prevNonEmpty)
                 , ("M-C-<Tab>", toggleWS)
                 , ("M-]", nextEmpty)
                 , ("M-[", prevEmpty)
                 , ("M-S-]", shiftToNext)
                 , ("M-S-[", shiftToPrev)
                 , ("M-S-`", windows $ W.shift "hide")
                 , ("M-`", windows $ W.greedyView "hide")

                 , ("M-\\", withFocused float)

                 , ("M-a", sendMessage Arrange)
                 , ("M-S-a", sendMessage DeArrange)

                 , ("M-<U>", sendMessage $ MoveUp 10)
                 , ("M-S-<U>", sendMessage $ IncreaseUp 10)
                 , ("M-C-<U>", sendMessage $ DecreaseUp 10)

                 , ("M-<L>", sendMessage $ MoveLeft 10)
                 , ("M-S-<L>", sendMessage $ IncreaseLeft 10)
                 , ("M-C-<L>", sendMessage $ DecreaseLeft 10)

                 , ("M-<R>", sendMessage $ MoveRight 10)
                 , ("M-S-<R>", sendMessage $ IncreaseRight 10)
                 , ("M-C-<R>", sendMessage $ DecreaseRight 10)

                 , ("M-<D>", sendMessage $ MoveDown 10)
                 , ("M-S-<D>", sendMessage $ IncreaseDown 10)
                 , ("M-C-<D>", sendMessage $ DecreaseDown 10)

                 , ("M-<Escape>", kill)
                 , ("M-S-<Escape>", kill1)
                 , ("M-c", windows copyToAll)
                 , ("M-S-c", killAllOtherCopies)
                 , ("M-C-d", removeWorkspace)
                 , ("M-'", selectWorkspace acPromptConfig)
                 , ("M-S-'", withWorkspace acPromptConfig (windows . W.shift))
                   -- Apps
                 , ("M-e", raiseMaybe (spawn "emacsclient -c") emacsQuery)
                 , ("M-S-e", spawn "emacsclient -c")
                 , ("M-S-m", raiseMaybe (spawn "emacs --name 'Wanderlust Mail' -wl") wanderlustQuery)
                 , ("M-f", runOrRaise "firefox" firefoxQuery)
                 , ("M-S-f", raiseMaybe (runInTerm "" "newsbeuter") newsbeuterQuery)
                 , ("M-i", raiseMaybe (runInTerm "" "weechat-curses") weechatQuery)
                   -- Layoutjumper
                 , ("M-<F1>", sendMessage $ JumpToLayout "Float")
                 , ("M-<F2>", sendMessage $ JumpToLayout "Two")
                 , ("M-<F3>", sendMessage $ JumpToLayout "Three")
                 , ("M-<F12>", sendMessage $ JumpToLayout "Full")
                 ]
                 ++ searchBindings
                 ++ scratchpadBindings

  where shutdown = "qdbus org.kde.ksmserver /KSMServer org.kde.KSMServerInterface.logout 1 2 0"
        logout = "qdbus org.kde.ksmserver /KSMServer org.kde.KSMServerInterface.logout 1 3 0"
        termExec = myTerm ++ " -e"
        dmenuOptions = buildOptions [ ("-fn", promptFont)
                                    , ("-nb", promptBG)
                                    , ("-sb", promptBG)
                                    , ("-nf", promptNFG)
                                    , ("-sf", promptSFG)
                                    ]
          where buildOptions = concatMap (\(flag, value) -> " " ++ flag ++ " '" ++ value ++ "'")
        withHome relativePath = homeDir ++ "/" ++ relativePath
        launcher = "cmd=$(yeganesh -- -p 'Run:'" ++ dmenuOptions ++ ") && $cmd"
        termLauncher = "cmd=$(yeganesh -p withTerm -- -p 'Run in Terminal:'"
                       ++ dmenuOptions ++ ") && " ++ termExec ++ " $cmd"
        respawn = "killall trayer ; xmonad --restart"
        nextNonEmpty = moveTo Next NonEmptyWS
        prevNonEmpty = moveTo Prev NonEmptyWS
        nextEmpty = moveTo Next EmptyWS
        prevEmpty = moveTo Prev EmptyWS
        shiftToNext = shiftTo Next EmptyWS
        shiftToPrev = shiftTo Prev EmptyWS
        acPromptConfig = promptConfig { autoComplete = Just 500000 }

-- PrettyPrinter ----------------------------------------
myPP h = defaultPP  { ppCurrent = xmobarColor "yellow" "black" . wrap "[" "]" 
                    , ppSep     = " :: "
                    , ppWsSep   = " "
                    , ppVisible = xmobarColor "#000000" "DarkSlateGrey"
                    , ppHidden  = xmobarColor "slateblue" "black"
                    , ppUrgent  = xmobarColor "#ffd700" "#b2222f" . xmobarStrip
                    , ppLayout  = xmobarColor "orange" "black" . wsRename
                    , ppTitle   = xmobarColor "green" "black" . wrap "[" "]" . shorten 80
                    , ppOutput  = hPutStrLn h
                    }
  where wsRename x = case x of
          "Mirror ResizableTall"   -> "MTiled"
          "ResizableTall"          -> "Tiled"
          _                        -> x

-- Prompt ----------------------------------------
promptFont = "xft:inconsolata:size=11:antialias=true:hinting=true:hintstyle=hintfull"
promptBG = "#171717"
promptNFG = "#ff7701"           -- non-selected Foreground
promptSFG = "#00aa4a"           -- selected Foreground
myPromptKeymap = M.union defaultXPKeymap $ M.fromList
                 [
                   ((controlMask, xK_g), quit)
                 , ((controlMask, xK_m), setSuccess True >> setDone True) 
                 , ((controlMask, xK_j), setSuccess True >> setDone True) 
                 , ((controlMask, xK_h), deleteString Prev)
                 , ((controlMask, xK_f), moveCursor Next)
                 , ((controlMask, xK_b), moveCursor Prev)
                 , ((controlMask, xK_p), moveHistory W.focusDown')
                 , ((controlMask, xK_n), moveHistory W.focusUp')
                 ]
promptConfig = defaultXPConfig { font = promptFont
                             , bgColor = promptBG
                             , fgColor = promptNFG
                             , bgHLight = promptBG
                             , fgHLight = promptSFG
                             , promptBorderWidth = 0
                             , height = 16
                             , historySize = 512
                             , historyFilter = deleteConsecutive
                             , promptKeymap = myPromptKeymap
                             }

-- Layouts ------------------------------------
twoPane = named "Two" $ TwoPane 0.04 0.5
threePane = named "Three" $ ThreeCol 1 0.04 0.4
centerGrid = named "CenterGrid" $ centerMaster Grid

myLayout = windowArrange $ smartBorders $ avoidStruts $
  onWorkspace "1:comm" (unevenTile ||| Grid ||| Full) $
  onWorkspace "hide" Grid $
  tiled ||| Mirror tiled ||| twoPane ||| threePane ||| centerGrid ||| float ||| Full
  where
    float = named "Float" simplestFloat
    unevenTile = ResizableTall 2 incDelta 0.8 []
    tiled = ResizableTall 1 incDelta goldenRatio []
    goldenRatio = toRational (2/(1 + sqrt 5 :: Double))
    incDelta = 0.04
----------------------------------------

-- Queries ----------------------------------------
prefixTitle prefix = (fmap (prefix `isPrefixOf`) title)
weechatQuery = prefixTitle "weechat"
emacsQuery = prefixTitle "emacs"
wanderlustQuery = title =? "Wanderlust Mail" 
newsbeuterQuery = title =? "newsbeuter"
firefoxQuery = className =? "Firefox"

-- Tie area ----------------------------------------
myManageHook = (composeAll . concat $
               [ [ isFullscreen    --> doFullFloat
                 , isDialog        --> doCenterFloat
                 , weechatQuery    --> doShift "1:comm"
                 , wanderlustQuery --> doShift "mail"
                 , newsbeuterQuery --> doShift "feeds"
                 ]
                ,[ className =? f --> doFloat            | f <- floats ]
                ,[ className =? f --> doCenterFloat      | f <- cfloats ]
                ,[ className =? c --> doShift "1:comm"   | c <- comms ]
                ,[ className =? b --> doShift "2:browse" | b <- browse ]
                ,[ className =? c --> doShift "3:code"   | c <- code ]
                ,[ className =? i --> doIgnore           | i <- ignores ]
                ,[ namedScratchpadManageHook scratchpads ]
                ])
               <+> manageDocks
  where ignores = []
        floats = ["Plasma-desktop", "Lancelot", "Kmix"]
        cfloats = ["MPlayer", "Smplayer", "Vlc", "Kaffeine"]
        browse = []
        code  = []
        comms = ["Kopete"]
----------------------------------------

scratchpads = [ NS "term" "urxvtcd -title term" (title =? "term") scratchFloat
              , NS "monitor" "urxvtcd -e htop" (title =? "htop") scratchFloat
              , NS "python" "urxvtcd -e ipython" (title =? "ipython") scratchFloat
              , NS "clojure" "urxvtcd -e clj" (title =? "clj") scratchFloat
              , NS "haskell" "urxvtcd -e ghci" (title =? "ghci") scratchFloat
              ]
  where scratchFloat = customFloating size
        size = W.RationalRect (1/4) (1/4) (1/2) (1/2)

scratchpadBindings = [ ("M-; t", namedScratchpadAction scratchpads "term")
                     , ("M-; h", namedScratchpadAction scratchpads "haskell")
                     , ("M-; p", namedScratchpadAction scratchpads "python")
                     , ("M-; c", namedScratchpadAction scratchpads "clojure")
                     , ("M-; m", namedScratchpadAction scratchpads "monitor")
                     ]

-- Search----------------------------------------
searchBindings = [("M-S-/ " ++ key, S.selectSearch engine) | (key, engine) <- searchList]
                 ++
                 [("M-/", S.promptSearch promptConfig multi)]
    where
      searchList = [ ("g", google)
                   , ("m", S.maps)
                   , ("i", imdb)
                   , ("w", S.wikipedia)
                   , ("d", wikiD)
                   , ("t", dict)
                   , ("l", leo)
                   ]
      multi = S.namedEngine "multi" $ foldr1 (!>) [ google
                                                  , scroogle
                                                  , wiki
                                                  , wikiD
                                                  , github
                                                  , bitbucket
                                                  , debbugs
                                                  , define
                                                  , dict 
                                                  , leo 
                                                  , S.maps
                                                  , images
                                                  , code
                                                  , pypi
                                                  , S.youtube
                                                  , zdfMediathek
                                                  , S.hoogle
                                                  , S.hackage
                                                  , imdb 
                                                  , S.alpha
                                                  , mathworld
                                                  , S.prefixAware google
                                                  ]
      -- new ones
      wikiD = S.searchEngine "wpd" "https://secure.wikimedia.org/wikipedia/de/wiki/Special:Search?go=Go&search="
      dict = S.searchEngine "dict" "http://www.dict.cc/?s="
      leo = S.searchEngine "leo" "http://dict.leo.org/ende?lp=ende&lang=de&searchLoc=0&cmpType=relaxed&sectHdr=on&spellToler=on&pinyin=diacritic&relink=on&search="
      imdb = S.searchEngine "imdb" "http://www.imdb.com/find?s=all&q="
      scroogle = S.searchEngine "s" "https://ssl.scroogle.org/cgi-bin/nbbwssl.cgi?Gw="
      google = S.searchEngine "g" "https://encrypted.google.com/search?q="
      define = S.searchEngine "def" "https://encrypted.google.com/search?q=define:"
      images = S.searchEngine "img" "http://images.google.com/images?q="
      pypi = S.searchEngine "pypi" "http://pypi.python.org/pypi?%3Aaction=search&submit=search&term="
      github = S.searchEngine "gh" "https://github.com/search?q="
      bitbucket = S.searchEngine "bb" "https://bitbucket.org/repo/all?name="
      debbugs = S.searchEngine "deb" "http://bugs.debian.org/"
      zdfMediathek = S.searchEngine "zdf" "http://www.zdf.de/ZDFmediathek/#/suche/"
      -- new names
      mathworld = S.namedEngine "math" S.mathworld
      code = S.namedEngine "code" S.codesearch
      wiki = S.namedEngine "wp" S.wikipedia

      (!>) :: SearchEngine -> SearchEngine -> SearchEngine
      (SearchEngine name1 site1) !> (SearchEngine name2 site2) =
        SearchEngine (name1 ++ "/" ++ name2) (\ s -> if (name1++":") `isPrefixOf` s
                                                    then site1 $ removeColonPrefix s
                                                    else site2 s)
        where removeColonPrefix = drop 1 . dropWhile (/= ':')
----------------------------------------
--  Local Variables:
--  compile-command: "xmonad --recompile"
--  End:
