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

import XMonad.Layout.NoBorders (noBorders, smartBorders)
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

import Data.List (isPrefixOf, findIndex)
import qualified Data.Map as M
import System.Directory (getHomeDirectory)
import System.Posix.Unistd (getSystemID, SystemID(..))
import Text.Printf

main = do
  homeDir <- getHomeDirectory
  systemID <- getSystemID
  xmproc <- spawnPipe $ xmobar systemID
  xmonad $ withUrgencyHook NoUrgencyHook $ ewmh $ defaultConfig { terminal = myTerm
                         , focusFollowsMouse = True
                         , borderWidth = 1
                         , normalBorderColor = "#000000"
                         , focusedBorderColor = "#9A0000"
                         , workspaces = ["1:comm", "2:browse", "3:code"] ++ map show [4..9] ++ ["mail", "feeds", "hide", "games"]
                         , modMask = mod4Mask -- use the Windows button as mod
                         , layoutHook = myLayout
                         , logHook = dynamicLogWithPP (myPP xmproc)
                         , handleEventHook = handleEventHook defaultConfig <+> fullscreenEventHook
                         , manageHook = myManageHook
                         , startupHook = myStartupHook
                         } `additionalKeysP` myKeys homeDir
    where
      myStartupHook = ewmhDesktopsStartup >> setWMName "LG3D"
      xmobar systemID = case (nodeName systemID) of
        "hitchhiker" -> "xmobar ~/.xmobar/laptop"
        "coficore"   -> "xmobar ~/.xmobar/desktop"
        _            -> "xmobar ~/.xmobar/default"

myTerm = "urxvtcd"
myKeys home = [ ("M-<Backspace>", spawn respawn)
                 , ("M-S-<Backspace>", spawn logout)
                 , ("M-C-<Backspace>", spawn shutdown)
                   -- Prompts/Launcher
                 , ("M-x", shellPromptHere promptConfig)
                 , ("M-S-x", spawn "krunner")
                 , ("M-p", shellPromptHere promptConfig)
                 , ("M-y", spawn launcher)
                 , ("M-S-y", spawn termLauncher)
                 , ("M-g", windowPromptGoto acPromptConfig)
                 , ("M-b", windowPromptBring acPromptConfig)
                 , ("M-S-b", windowPromptBringCopy acPromptConfig)
                 , ("M-z", manPrompt promptConfig)
                 , ("M-q", dirExecPromptNamed acPromptConfig spawn (withHome ".quick") "Quicks: ")
                 , ("M-S-q", dirExecPromptNamed acPromptConfig spawn (withHome ".quick") "Quicks: ")
                 , ("M-S-g", dirExecPromptNamed acPromptConfig spawn (withHome "Games/bin") "Game: ")
                   -- Window/workspace management
                 , ("M-S-h", sendMessage MirrorShrink)
                 , ("M-S-l", sendMessage MirrorExpand)
                 , ("M-u", focusUrgent)
                 , ("M-S-u", clearUrgents)
                 , ("M-S-t", sinkAll)
                   -- movement
                 , ("M-<Tab>", nextNonEmpty)
                 , ("M-S-<Tab>", prevNonEmpty)
                 , ("M-C-l", nextNonEmpty)
                 , ("M-C-h", prevNonEmpty)
                 , ("M-C-j", shiftToNext)
                 , ("M-C-k", shiftToPrev)
                 , ("M-C-<Tab>", toggleWS)
                 , ("M-]", nextEmpty)
                 , ("M-[", prevEmpty)
                 , ("M-S-]", shiftToNextEmpty)
                 , ("M-S-[", shiftToPrevEmpty)
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

                 , ("M-C-1", screenWorkspace 0 >>= flip whenJust (windows . W.view))
                 , ("M-C-2", screenWorkspace 1 >>= flip whenJust (windows . W.view))
                 , ("M-C-3", screenWorkspace 2 >>= flip whenJust (windows . W.view))
--                  , ("M-d", spawn "disper -C")
                 , ("M-d", spawn "xrandr --output DP-0 --auto --left-of LVDS-0")

                 , ("M-<Escape>", kill)
                 , ("M-S-<Escape>", kill1)
                 , ("M-C-<Escape>", spawn "xkill")
                 , ("M-c", windows copyToAll)
                 , ("M-S-c", killAllOtherCopies)
                 , ("M-C-d", removeWorkspace)
                 , ("M-'", selectWorkspace promptConfig)
                 , ("M-S-'", withWorkspace promptConfig (windows . W.shift))
                   -- Apps
                 , ("M-e", raiseMaybe (spawn "emacsclient -c") emacsQuery)
                 , ("M-S-e", spawn "emacsclient -c")
                 , ("M-S-m", raiseMaybe (spawn "gnus") gnusQuery)
                 , ("M-f", raiseMaybe (spawnOn "2:browse" "firefox") firefoxQuery)
                 , ("M-S-f", raise elfeedQuery)
                   -- Layoutjumper
                 , ("M-<F1>", sendMessage $ JumpToLayout "Float")
                 , ("M-<F2>", sendMessage $ JumpToLayout "Two")
                 , ("M-<F3>", sendMessage $ JumpToLayout "Three")
                 , ("M-<F12>", sendMessage $ JumpToLayout "Full")
                 , ("M-s", sendMessage $ ToggleStruts)
                 -- volume
                 , ("<XF86AudioRaiseVolume>", spawn "pads up 5")
                 , ("<XF86AudioLowerVolume>", spawn "pads down 5")
                 , ("<XF86AudioMute>", spawn "pads mute")
                 , ("S-<XF86AudioRaiseVolume>", spawn "pads in-up 5")
                 , ("S-<XF86AudioLowerVolume>", spawn "pads in-down 5")
                 , ("S-<XF86AudioMute>", spawn "pads in-mute")
                 ]
                 ++ searchBindings
                 ++ scratchpadBindings
                 ++ programBindings
                 ++ mpdBindings

  where shutdown = "qdbus org.kde.ksmserver /KSMServer org.kde.KSMServerInterface.logout 1 2 0"
        logout = "qdbus org.kde.ksmserver /KSMServer org.kde.KSMServerInterface.logout 1 3 0"
        termExec = myTerm ++ " -e "
        dmenuOptions = buildOptions [ ("-fn", promptFont)
                                    , ("-nb", promptBG)
                                    , ("-sb", promptBG)
                                    , ("-nf", promptNFG)
                                    , ("-sf", promptSFG)
                                    ]
          where buildOptions = concatMap (\(flag, value) -> " " ++ flag ++ " '" ++ value ++ "'")
        withHome relativePath = home ++ "/" ++ relativePath
        launcher = "cmd=$(yeganesh -- -p 'Run:'" ++ dmenuOptions ++ ") && $cmd"
        termLauncher = "cmd=$(yeganesh -p withTerm -- -p 'Run in Terminal:'"
                       ++ dmenuOptions ++ ") && " ++ termExec ++ "$cmd"
        respawn = "xmonad --restart"
        nextNonEmpty = moveTo Next NonEmptyWS
        prevNonEmpty = moveTo Prev NonEmptyWS
        nextEmpty = moveTo Next EmptyWS
        prevEmpty = moveTo Prev EmptyWS
        shiftToNextEmpty = shiftTo Next EmptyWS
        shiftToPrevEmpty = shiftTo Prev EmptyWS
        acPromptConfig = promptConfig { autoComplete = Just 500000 }
        programBindings = prefixKeymap "M-o" [ ("m", spawn "smplayer")
                                             , ("d", spawn "dolphin")
                                             , ("o", spawn "okular")
                                             , ("c", spawn "calibre")
                                             , ("a", spawn "amarok")
                                             ]
        mpdBindings = prefixKeymap "M-S-p" [ ("l", spawn "mpc volume -10")
                                           , ("h", spawn "mpc volume +10")
                                           , ("s", spawn "mpc stop")
                                           , ("<Space>", spawn "mpc toggle")
                                           , ("n", spawn "mpc next")
                                           , ("p", spawn "mpc prev")
                                           , ("o", spawn "mpd")
                                           , ("k", spawn "mpd --kill")
                                           , ("q", spawn "qmpdclient")
                                           , ("<XF86AudioRaiseVolume>", spawn "mpc volume +10")
                                           , ("<XF86AudioLowerVolume>", spawn "mpc volume -10")
                                           ]
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
                 , ((mod1Mask, xK_p), moveHistory W.focusDown')
                 , ((mod1Mask, xK_n), moveHistory W.focusUp')
                 , ((mod1Mask, xK_b), moveWord Prev)
                 , ((mod1Mask, xK_f), moveWord Next)
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
  onWorkspace "1:comm" (unevenTile ||| Grid ||| full) $
  onWorkspace "hide" Grid $
  tiled ||| Mirror tiled ||| twoPane ||| threePane ||| centerGrid ||| float ||| full
  where
    float = named "Float" simplestFloat
    unevenTile = ResizableTall 2 incDelta 0.8 []
    tiled = ResizableTall 1 incDelta goldenRatio []
    goldenRatio = toRational (2/(1 + sqrt 5 :: Double))
    incDelta = 0.04
    full = noBorders Full
----------------------------------------

-- Queries ----------------------------------------
prefixTitle prefix = fmap (prefix `isPrefixOf`) title
emacsQuery = prefixTitle "emacs"
gnusQuery = title =? "Gnus Mail"
elfeedQuery = title =? "Elfeed"
firefoxQuery = className =? "Firefox"

-- Tie area ----------------------------------------
myManageHook = (composeAll . concat $
               [ [ isFullscreen    --> doFullFloat
                 , isDialog        --> doCenterFloat
                 , gnusQuery       --> doShift "mail"
                 ]
                ,[ className =? f --> doFloat            | f <- floats ]
                ,[ className =? f --> doCenterFloat      | f <- cfloats ]
                ,[ className =? c --> doShift "1:comm"   | c <- comms ]
                ,[ className =? b --> doShift "2:browse" | b <- browse ]
                ,[ className =? c --> doShift "3:code"   | c <- code ]
                ,[ className =? i --> doIgnore           | i <- ignores ]
                ,[ className =? g --> doShift "games"    | g <- games ]
                ,[ namedScratchpadManageHook scratchpads ]
                ])
               <+> manageDocks
  where ignores = []
        floats = ["Plasma-desktop", "Kmix", "Klipper"]
        cfloats = ["MPlayer", "Smplayer", "Vlc", "Kaffeine"]
        games = ["ck2"]
        browse = []
        code  = []
        comms = []
----------------------------------------

scratchpads = [ NS "term" "urxvtcd -title term" (title =? "term") scratchFloat
              , NS "monitor" "urxvtcd -e htop" (title =? "htop") scratchFloat

              , NS "python" "urxvtcd -e ipython" (title =? "ipython") scratchFloat
              , NS "clojure" "urxvtcd -e clj" (title =? "clj") scratchFloat
              , NS "haskell" "urxvtcd -e ghci" (title =? "ghci") scratchFloat
              , NS "scala" "urxvtcd -title scala -e scala" (title =? "scala") scratchFloat
              , NS "lisp" "urxvtcd -title lisp -e sbcl" (title =? "lisp") scratchFloat
              , NS "lua" "urxvtcd -title lua -e lua" (title =? "lua") scratchFloat
              , NS "r" "urxvtcd -title rpad -e R" (title =? "rpad") scratchFloat

              , NS "capture" "org-capture" (title =? "Capture Frame") orgFloat
              , NS "agenda" "org-agenda" (title =? "Agenda Frame") orgFloat
              , NS "calc" "emacsclient -a '' -c -e '(cofi-full-calc)'" (title =? "calc") scratchFloat
              , NS "dict" "urxvtcd -title dict -e pdictcc --limit 5" (title =? "dict") scratchFloat
              , NS "tmux" "urxvtcd -e sh -c 'tmux attach -t tmux || tmux new -s tmux'" (prefixTitle "tmux") scratchFloat
              ]
  where scratchFloat = customFloating size
        orgFloat = customFloating orgsize
        size = W.RationalRect (1/4) (1/4) (1/2) (1/2)
        orgsize = W.RationalRect (1/2) (1/2) (1/2) (1/2)

scratchpadBindings = prefixKeymap "M-;" [ ("t", namedScratchpadAction scratchpads "term")
                                        , ("h", namedScratchpadAction scratchpads "haskell")
                                        , ("p", namedScratchpadAction scratchpads "python")
                                        , ("c", namedScratchpadAction scratchpads "calc")
                                        , ("C", namedScratchpadAction scratchpads "clojure")
                                        , ("l", namedScratchpadAction scratchpads "lisp")
                                        , ("S-l", namedScratchpadAction scratchpads "lua")
                                        , ("m", namedScratchpadAction scratchpads "monitor")
                                        , ("o", namedScratchpadAction scratchpads "capture")
                                        , ("a", namedScratchpadAction scratchpads "agenda")
                                        , ("d", namedScratchpadAction scratchpads "dict")
                                        , ("x", namedScratchpadAction scratchpads "tmux")
                                        , ("r", namedScratchpadAction scratchpads "r")
                                        , ("s", namedScratchpadAction scratchpads "scala")
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
                                                  , scholar
                                                  , acm
                                                  , ieee
                                                  , ixquick
                                                  , wiki
                                                  , wikiD
                                                  , amazon
                                                  , play
                                                  , github
                                                  , bitbucket
                                                  , sourceforge
                                                  , emacs
                                                  , debbugs
                                                  , define
                                                  , urbanDictionary
                                                  , dict
                                                  , leo
                                                  , thesaurus
                                                  , S.maps
                                                  , images
                                                  , pypi
                                                  , pep
                                                  , py
                                                  , nullege
                                                  , lispdoc
                                                  , rtd
                                                  , S.youtube
                                                  , zdfMediathek
                                                  , S.hoogle
                                                  , S.hackage
                                                  , imdb
                                                  , pgpmit
                                                  , S.alpha
                                                  , mathworld
                                                  , ctan
                                                  , rfc
                                                  , duden
                                                  , goodreads
                                                  , wh
                                                  , curse
                                                  , bnet
                                                  , google
                                                  , S.prefixAware duck
                                                  ]

      -- new ones
      wiki = S.searchEngine "wp" "http://en.wikipedia.org/wiki/Special:Search?go=Go&search="
      wikiD = S.searchEngine "wpd" "http://de.wikipedia.org/wiki/Special:Search?go=Go&search="
      dict = S.searchEngine "dict" "http://www.dict.cc/?s="
      leo = S.searchEngine "leo" "http://dict.leo.org/ende?lp=ende&lang=de&searchLoc=0&cmpType=relaxed&sectHdr=on&spellToler=on&pinyin=diacritic&relink=on&search="
      imdb = S.searchEngine "imdb" "http://www.imdb.com/find?s=all&q="
      ixquick = S.searchEngine "i" "https://ixquick.com/do/search?q="
      google = S.searchEngine "g" "https://encrypted.google.com/search?q="
      define = S.searchEngine "def" "https://encrypted.google.com/search?q=define:"
      images = S.searchEngine "img" "http://images.google.com/images?q="
      pypi = S.searchEngine "pypi" "http://pypi.python.org/pypi?%3Aaction=search&submit=search&term="
      github = S.searchEngine "gh" "https://github.com/search?q="
      bitbucket = S.searchEngine "bb" "https://bitbucket.org/repo/all?name="
      debbugs = S.searchEngine "deb" "http://bugs.debian.org/"
      zdfMediathek = S.searchEngine "zdf" "http://www.zdf.de/ZDFmediathek/#/suche/"
      pgpmit = S.searchEngine "pgp" "http://pgp.mit.edu:11371/pks/lookup?search="
      alpha = S.searchEngine "alpha" "http://www.wolframalpha.com/input/i="
      urbanDictionary = S.searchEngine "ud" "http://www.urbandictionary.com/define.php?term="
      ctan = S.searchEngine "ctan" "http://www.ctan.org/search/?search_type=description&search_type=filename&search_type=id&search="
      duck = S.searchEngine "d" "http://duckduckgo.com/?q="
      rfc = S.searchEngine "rfc" "http://www.ietf.org/rfc/rfc"
      pep = formatSearch "pep" "http://www.python.org/dev/peps/pep-%04s"
      rtd = S.searchEngine "rtd" "http://readthedocs.org/search/project/?q="
      py = S.searchEngineF "py" dopy
      nullege = S.searchEngine "null" "http://nullege.com/codes/search/"
      lispdoc = S.searchEngine "ld" "http://lispdoc.com/?q="
      amazon = S.searchEngine "ama" "http://www.amazon.de/s/url=search-alias%3Daps&x=0&y=0&field-keywords="
      play = S.searchEngine "play" "https://play.google.com/store/search?q="
      duden = S.searchEngine "duden" "http://www.duden.de/suchen/dudenonline/"
      sourceforge = S.searchEngine "sf" "http://sourceforge.net/search/?q="
      scholar = S.searchEngine "scholar" "https://scholar.google.de/scholar?q="
      acm = S.searchEngine "acm" "https://dl.acm.org/results.cfm?query="
      ieee= S.searchEngine "ieee" "http://ieeexplore.ieee.org/search/searchresult.jsp?queryText="
      thesaurus = S.searchEngine "th" "http://thesaurus.com/browse/"
      emacs = S.searchEngine "emacs" "http://www.emacswiki.org/cgi-bin/wiki?search="
      goodreads = S.searchEngine "goodreads" "https://www.goodreads.com/search?query="
      wh = S.searchEngine "wow" "http://www.wowhead.com/search?q="
      curse = S.searchEngine "curse" "http://www.curse.com/search/addons?game-slug=wow&search="
      bnet = S.searchEngine "bnet" "http://eu.battle.net/wow/en/search?q="
      -- new names
      mathworld = S.namedEngine "math" S.mathworld

      (!>) :: SearchEngine -> SearchEngine -> SearchEngine
      (SearchEngine name1 site1) !> (SearchEngine name2 site2) =
        SearchEngine (name1 ++ "/" ++ name2) (\ s -> if (name1++":") `isPrefixOf` s
                                                    then site1 $ removeColonPrefix s
                                                    else site2 s)
        where removeColonPrefix = drop 1 . dropWhile (/= ':')
      formatSearch name fstring = S.searchEngineF name (\s -> (printf fstring s) :: String)
      dopy s = case f of
                 ""    -> "http://python.org"
                 "doc" -> "http://docs.python.org/" ++ r
                 "doc3" -> "http://docs.python.org/py3k/" ++ r
                 "lib" -> "http://docs.python.org/library/" ++ r ++ ".html"
                 "lib3" -> "http://docs.python.org/py3k/library/" ++ r ++ ".html"
              where a = split '/' s
                    f = if (length a > 1)
                        then head a
                        else ""
                    r = if (length a > 1)
                        then concat $ tail a
                        else concat a

prefixKeymap prefix mapping = map (\ (binding, comm) -> (prefix ++ " " ++ binding, comm)) mapping

split :: (Eq a) => a -> [a] -> [[a]]
split x [] = [[]]
split x xs = case findIndex (\x' -> x' == x) xs of
               Just num -> take num xs : (split x (drop (num + 1) xs))
               Nothing -> [xs]
----------------------------------------
--  Local Variables:
--  compile-command: "xmonad --recompile"
--  End:
