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
import           XMonad.Actions.CopyWindow      ( copy )

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
    , ((myMod .|. shiftMask, xK_l), spawn "slock")
    ]

    ++ [ ( (myMod, xK_p)
         , spawn
           "rofi -combi-modi window,drun,emoji -theme solarized -show combi -modi combi,run,calc -terse -no-show-match -no-sort -location 1 -width 100"
         )
       , ((0, xF86XK_AudioRaiseVolume), spawn "pulseaudio-ctl up")
       , ((0, xF86XK_AudioLowerVolume), spawn "pulseaudio-ctl down")
       , ((0, xF86XK_AudioMute)       , spawn "pulseaudio-ctl mute")
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
         , spawn "sudo light -A 5"
         ) -- Added to visudo, deb manually installed
       , ((0, xF86XK_MonBrightnessDown), spawn "sudo light -U 5")
       , ((myMod, xK_Print), spawn "sh ~/.xmonad/scripts/select-screenshot.sh")
       , ((myMod, xK_f), spawn "nautilus")
       , ( (myMod .|. shiftMask, xK_h)
         , spawn
           "rofi -modi 'clipboard:greenclip print' -theme solarized -show clipboard -terse -no-show-match -no-sort -location 1 -width 100 -run-command '{cmd}'"
         )
       , ( (modm, xK_a)
         , sequence_ $ [ windows $ copy i | i <- XMonad.workspaces conf ]
         )    -- Pin to all workspaces
       , ((modm .|. shiftMask, xK_a), windows $ kill8)    -- remove from all but current
       ]

    -- Screen order for triple screens.
    ++ [ ( (m .|. myMod, key)
         , screenWorkspace sc >>= flip whenJust (windows . f)
         )
       | (key, sc) <- zip [xK_w, xK_e, xK_r] [1, 0, 2]
       , (f  , m ) <- [(W.greedyView, 0), (W.shift, shiftMask)]
       ]

myRemovedKeys = [((myMod .|. shiftMask, xK_q))]

myLayoutHook =
  avoidStruts
    $ smartBorders
    $ (   (Tall 1 (3 / 100) (1 / 2))
      ||| Grid
      ||| ThreeCol 1 (3 / 100) (1 / 3)
      ||| Full
      )

myConfig =
  ewmh
    $ def { terminal           = "urxvt"
          , modMask            = mod4Mask
          , startupHook        = myStartupHook
          , manageHook         = manageSpawn <+> manageHook def
          , focusedBorderColor = "#fb9224"
          , normalBorderColor  = "#000"
          , borderWidth        = 2
          , logHook            = myEventLogHook
          , layoutHook         = myLayoutHook
          , handleEventHook    = handleEventHook def <+> fullscreenEventHook
          , keys               = myKeys
          }
    `removeKeys` myRemovedKeys

myStartupHook = do
  spawnOnOnce "8" "todoist"
  spawnOnOnce "8" "spotify"
  spawnOnOnce "2" "code"
  spawnOnOnce "2" "urxvt"
  spawnOnOnce "1" "firefox"
  spawnOnOnce "9" "slack"
  spawnOnOnce "9" "thunderbird"

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

main = xmonad =<< myStatusBar myConfig


-- TODO: How do you resize floating windows?
-- TODO: spawnOnOnce steals focus at startup
