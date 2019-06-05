import           XMonad                  hiding ( (|||) )
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout           hiding ( (|||) )
import           XMonad.Layout.Grid
import           XMonad.Layout.ThreeColumns
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
import           System.IO
import           XMonad.Hooks.EwmhDesktops      ( ewmh
                                                , fullscreenEventHook
                                                )
import           XMonad.Hooks.ManageDocks
import           XMonad.Util.EZConfig
import           Graphics.X11.ExtraTypes.XF86
import           XMonad.Actions.PhysicalScreens
import           XMonad.Hooks.SetWMName
import           XMonad.Actions.OnScreen
import           XMonad.Actions.SpawnOn
import           XMonad.Util.SpawnOnce
import qualified Data.Map                      as M

myBar = "killall -q polybar; polybar xmother"

myStatusBar = statusBar myBar (PP { ppOutput = \s -> return () }) def

myMod = mod4Mask
altMask = mod1Mask

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

-- Use this to detect keys
-- xev | grep -A2 --line-buffered '^KeyRelease' | sed -n '/keycode /s/^.*keycode \([0-9]*\).* (.*, \(.*\)).*$/\1 \2/p'
myKeys x = M.union (M.fromList (newKeys x)) (keys def x)
newKeys conf@(XConfig { XMonad.modMask = modm }) =
  [ ( (myMod, xK_b)
    , sequence_ [spawn "polybar-msg cmd toggle", sendMessage ToggleStruts]
    )
    , ((myMod .|. shiftMask, xK_l), spawn "xscreensaver-command -lock")
    ]

    ++ [ ( (myMod, xK_p)
         , spawn
           "rofi -combi-modi window,drun,emoji -theme solarized -show combi -modi combi,run,calc -terse -no-show-match -no-sort -location 1 -width 100"
         )
       , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume 0 +5%")
       , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume 0 -5%")
       , ((0, xF86XK_AudioMute)       , spawn "pactl set-sink-mute 0 toggle")
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
       , ((myMod, xK_Print), spawn "sh ~/.xmonad/scripts/select-screenshot.sh")
       ]

    -- Screen order for triple screens.
    ++ [ ( (m .|. myMod, key)
         , screenWorkspace sc >>= flip whenJust (windows . f)
         )
       | (key, sc) <- zip [xK_w, xK_e, xK_r] [1, 0, 2]
       , (f  , m ) <- [(W.greedyView, 0), (W.shift, shiftMask)]
       ]

    -- Use greedyView when mod + ctrl + workspace
    ++ [ ((m .|. myMod, k), windows (f i))
       | (i, k) <- zip (workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
       , (f, m) <- [(W.greedyView, controlMask), (W.view, myMod)]
       ]

    -- Use view when mod + workspace
    ++ [ ((m .|. myMod, k), windows (f i))
       | (i, k) <- zip (workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
       , (f, m) <- [(W.view, myMod), (W.view, myMod)]
       ]


myRemovedKeys = [((myMod .|. shiftMask, xK_q))]

myLayoutHook =
  avoidStruts
    $ smartBorders
    $ ((Tall 1 (3 / 100) (1 / 2)) ||| Grid ||| ThreeCol 1 (3 / 100) (1 / 3))

myConfig =
  ewmh
    $ def { terminal           = "termite"
          , modMask            = mod4Mask
          , startupHook        = myStartupHook
          , manageHook         = manageSpawn <+> manageHook def
          , focusedBorderColor = "#00d6d6"
          , borderWidth        = 2
          , logHook            = myEventLogHook
          , layoutHook         = myLayoutHook
          , handleEventHook    = handleEventHook def <+> fullscreenEventHook
          , keys               = myKeys
          }
    `removeKeys` myRemovedKeys

myStartupHook = do
  spawnOnOnce "1" "todoist"
  spawnOnOnce "1" "spotify"
  spawnOnOnce "2" "code"
  spawnOnOnce "2" "termite"
  spawnOnOnce "3" "firefox"
  spawnOnOnce "9" "slack"
  spawnOnOnce "9" "thunderbird"

myEventLogHook = do
  forM_ [".xmonad-workspace-log", ".xmonad-title-log", ".xmonad-layout-log"]
    $ \file -> do
        safeSpawn "mkfifo" ["/tmp/" ++ file]
  winset <- gets windowset
  title  <- maybe (return "") (fmap show . getName) . W.peek $ winset
  let currWs  = W.currentTag winset
  let currWsM = W.lookupWorkspace currWs
  let wss     = map W.tag $ W.workspaces winset
  let wsStr   = join $ map (fmt currWs) $ sort' wss
  let lStr = description . W.layout . W.workspace . W.current $ winset

  io $ appendFile "/tmp/.xmonad-title-log" (title ++ "\n")
  io $ appendFile "/tmp/.xmonad-workspace-log" (wsStr ++ "\n")
  io $ appendFile "/tmp/.xmonad-layout-log" (lStr ++ "\n")


 where
  fmt currWs ws | currWs == ws = "[" ++ ws ++ "]"
                | otherwise    = " " ++ ws ++ " "
  sort' = sortBy (compare `on` (!! 0))


main = xmonad =<< myStatusBar myConfig




