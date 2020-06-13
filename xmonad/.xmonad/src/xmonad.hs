import           XMonad                  hiding ( (|||) )
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.Grid
import qualified XMonad.Prompt                 as P
import qualified XMonad.Actions.Submap         as SM
import qualified XMonad.Actions.Search         as S
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.DwmStyle
import           Data.List                      ( sortBy )
import           Data.Function                  ( on )
import           Control.Monad                  ( forM_
                                                , join
                                                )
import           XMonad.Layout.NoBorders        ( smartBorders )
import           XMonad.Util.Run                ( safeSpawn )
import           XMonad.Util.NamedWindows       ( getName )
import           XMonad.Hooks.DynamicLog
import qualified XMonad.StackSet               as W
import           XMonad.Hooks.EwmhDesktops      ( ewmh
                                                , fullscreenEventHook
                                                )
import           XMonad.Hooks.ManageDocks
import           XMonad.Util.EZConfig
import           Graphics.X11.ExtraTypes.XF86
import           XMonad.Hooks.SetWMName
import           XMonad.Actions.SpawnOn
import           XMonad.Util.SpawnOnce
import qualified Data.Map                      as M
import           XMonad.Actions.CopyWindow      ( copy )

myBar = "killall -q polybar; polybar xmother"
myStatusBar = statusBar myBar (PP { ppOutput = \s -> return () }) def
myTerminal = "gnome-terminal"
myMod = mod4Mask -- Super Key
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]



searchEngineMap method =
  M.fromList
    $ [ ((0, xK_a), method S.alpha)
      , ((0, xK_d), method S.duckduckgo)
      , ((0, xK_i), method S.imdb)
      , ((0, xK_y), method S.youtube)
      , ((0, xK_m), method S.maps)
      , ((0, xK_g), method S.google)
      , ((0, xK_h), method S.hoogle)
      , ((0, xK_w), method S.wikipedia)
      ]

-- Use this to detect keys
-- xev | grep -A2 --line-buffered '^KeyRelease' | sed -n '/keycode /s/^.*keycode \([0-9]*\).* (.*, \(.*\)).*$/\1 \2/p'
myKeys x = M.union (M.fromList (newKeys x)) (keys def x)
newKeys conf@XConfig { XMonad.modMask = modm } =
  [ ( (modm, xK_b)
    , sequence_ [spawn "polybar-msg cmd toggle", sendMessage ToggleStruts]
    )
    , ((modm .|. shiftMask, xK_l), spawn "slock")
    ]

    ++ [ ( (modm, xK_p)
         , spawn
           "rofi -combi-modi window,drun,run,emoji -theme solarized -show combi -modi combi,run -terse -no-show-match -no-sort -location 1 -width 100"
         )
       , ((modm, xK_v), spawn "DESKTOP_SESSION=kde pavucontrol -t 3")
       , ((modm, xK_c), spawn "blueman-manager")
       , ( (modm, xK_u)
         , spawn
           "unipicker --copy --command 'rofi -dmenu -theme solarized -location 1 -width 100'"
         )
       , ( (0, xF86XK_AudioRaiseVolume)
         , spawn "~/.wm-scripts/media.sh volume-inc"
         )
       , ( (0, xF86XK_AudioLowerVolume)
         , spawn "~/.wm-scripts/media.sh volume-dec"
         )
       , ((0, xF86XK_AudioMute), spawn "~/.wm-scripts/media.sh mute")
       , ( (0, xF86XK_AudioPause)
         , spawn
           "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Pause"
         )
       , ( (0, xF86XK_AudioPlay)
         , spawn
           "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Play"
         )
       , ( (0, xF86XK_AudioNext)
         , spawn
           "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next"
         )
       , ( (0, xF86XK_AudioPrev)
         , spawn
           "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous"
         )
       , ( (0, xF86XK_MonBrightnessUp)
         , spawn "~/.wm-scripts/media.sh brightness-inc"
         ) -- Added to visudo, deb manually installed
       , ( (0, xF86XK_MonBrightnessDown)
         , spawn "~/.wm-scripts/media.sh brightness-dec"
         )
       , ((0, xK_Print), spawn "flameshot gui")
       , ((modm, xK_f), spawn "XDG_CURRENT_DESKTOP=kde dolphin")
       , ( (modm .|. shiftMask, xK_h)
         , spawn
           "rofi -modi 'clipboard:greenclip print' -theme skill8olarized -show clipboard -terse -no-show-match -no-sort -location 1 -width 100 -run-command '{cmd}'"
         )
       , ( (modm, xK_a)
         , sequence_ $ [ windows $ copy i | i <- XMonad.workspaces conf ]
         )    -- Pin to all workspaces
       , ( (modm .|. shiftMask, xK_a)
         , windows $ kill8
         )    -- remove from all but current
       , ((modm, xK_s), SM.submap $ searchEngineMap $ S.promptSearch P.def)
       , ( (modm .|. shiftMask, xK_s)
         , SM.submap $ searchEngineMap $ S.selectSearch
         )
       ]
    -- Screen order for triple screens.
    ++ [ ( (m .|. modm, key)
         , screenWorkspace sc >>= flip whenJust (windows . f)
         )
       | (key, sc) <- zip [xK_w, xK_e, xK_r] [0, 1, 2]
       , (f  , m ) <- [(W.greedyView, 0), (W.shift, shiftMask)]
       ]

    -- ++ [((myMod, k), (S.promptSearch P.def f)) | (k,f) <- searchList ]

myRemovedKeys = [(myMod .|. shiftMask, xK_q)]

myLayoutHook =
  avoidStruts
    $ smartBorders
    $ dwmStyle shrinkText def
    $ (   (Tall 1 (3 / 100) (1 / 2))
      ||| Grid
      ||| ThreeCol 1 (3 / 100) (1 / 3)
      ||| Full
      )

myManageHook = composeAll
  [ title =? "Emulator" --> doFloat
  , title =? "Android Emulator - pixel:5554" --> doFloat
  ]

myStartupHook = do
  spawnOnOnce "8" "spotify"
  spawnOnOnce "2" "code"
  spawnOnOnce "1" "firefox"
  spawnOnOnce "7" "joplin"
  spawnOnOnce "9" "slack"
  spawnOnOnce "9" "thunderbird"
  setWMName "LG3D"

myEventLogHook = do
  forM_ [".xmonad-workspace-log", ".xmonad-title-log", ".xmonad-layout-log"]
    $ \file -> safeSpawn "mkfifo" ["/tmp/" ++ file]
  winset <- gets windowset
  title  <- maybe (return "") (fmap show . getName) . W.peek $ winset
  let currWs = W.currentTag winset
  let activeWss = filter
        (\ws -> (length (W.stack ws) > 0) || W.tag ws == currWs)
        (W.workspaces winset)
  let wsTags = map W.tag $ activeWss
  let wsStr  = join $ map (fmt currWs) $ sort' wsTags
  let lStr = description . W.layout . W.workspace . W.current $ winset

  io $ appendFile "/tmp/.xmonad-title-log" (title ++ "\n")
  io $ appendFile "/tmp/.xmonad-workspace-log" (wsStr ++ "\n")
  io $ appendFile "/tmp/.xmonad-layout-log" (lStr ++ "\n")

 where
  fmt currWs ws | currWs == ws = "[" ++ ws ++ "]"
                | otherwise    = " " ++ ws ++ " "
  sort' = sortBy (compare `on` (!! 0))


kill8 ss | Just w <- W.peek ss = (W.insertUp w) $ W.delete w ss
         | otherwise           = ss


myConfig =
  ewmh
    $ def { terminal           = myTerminal
          , modMask            = myMod
          , startupHook        = myStartupHook
          , manageHook         = manageSpawn <+> myManageHook <+> manageHook def
          , focusedBorderColor = "#fb9224"
          , normalBorderColor  = "#000"
          , borderWidth        = 3
          , logHook            = myEventLogHook
          , layoutHook         = myLayoutHook
          , handleEventHook    = handleEventHook def <+> fullscreenEventHook
          , keys               = myKeys
          }
    `removeKeys` myRemovedKeys

main = xmonad =<< myStatusBar myConfig


-- TODO: spawnOnOnce steals focus at startup
