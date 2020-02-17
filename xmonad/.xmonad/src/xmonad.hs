import           XMonad                  hiding ( (|||) )
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.Grid
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.DwmStyle
import           Data.List                      ( sortBy )
import           Data.Function                  ( on )
import           Data.Monoid
import           Control.Monad                  ( forM_
                                                , join
                                                )
import           XMonad.Layout.NoBorders        ( smartBorders )
import           XMonad.Util.Run                ( safeSpawn )
import           XMonad.Util.NamedWindows       ( getName )
import           XMonad.Hooks.DynamicLog
import qualified XMonad.StackSet               as W
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Util.EZConfig
import           Graphics.X11.ExtraTypes.XF86
import           XMonad.Actions.SpawnOn
import           XMonad.Actions.OnScreen
import           XMonad.Util.SpawnOnce
import qualified Data.Map                      as M
import           Data.Maybe
import           XMonad.Actions.CopyWindow      ( copy )
import           XMonad.Actions.UpdateFocus
import XMonad.Layout.MagicFocus

myBar :: String
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
           "rofi -combi-modi window,drun,emoji -theme solarized -show combi -modi combi,run,calc -terse -no-show-match  -calc-command \"echo '{result}' | xsel -b\" -no-sort -location 1 -width 100"
         )
       , ((myMod, xK_v), spawn "DESKTOP_SESSION=kde pavucontrol -t 3")
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
       , ((myMod, xK_Print), spawn "sh ~/.wm-scripts/select-screenshot.sh")
       , ((myMod, xK_f)    , spawn "XDG_CURRENT_DESKTOP=kde dolphin")
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
    $ dwmStyle shrinkText def
    $ (   (Tall 1 (3 / 100) (1 / 2))
      ||| Grid
      ||| ThreeCol 1 (3 / 100) (1 / 3)
      ||| Full
      )

myTerminal = "kitty"




myEwmhDesktopsEventHook :: Event -> X All
myEwmhDesktopsEventHook e@ClientMessageEvent { ev_window = w, ev_message_type = mt }
  = withWindowSet $ \ws -> do -- ws is the current stackset
    a_aw <- getAtom "_NET_ACTIVE_WINDOW"
    if mt == a_aw
      then do
        let curWsId = W.current ws 
            currentScreenId = (W.screen . W.current) ws -- the current screen
            selectedWs = 0 
            wsid = (W.tag . W.workspace) curWsId 
            st =  W.integrate' . W.stack . W.workspace $ W.current ws
        e <- gets (fromMaybe 0 . M.lookup w . waitingUnmap)
        _ <- windows (greedyViewOnScreen currentScreenId wsid)
        windows (W.swapMaster . W.focusWindow w)
        -- windows $ greedyViewOnScreen 1 "1" 
        spawn "notify-send 'Curr Screen'"
        -- windows $ viewOnScreen 1 "1"
        return (All True)
      else ewmhDesktopsEventHook e
myEwmhDesktopsEventHook e = ewmhDesktopsEventHook e


-- | Add EWMH functionality to the given config.  See above for an example.
ewmh' :: XConfig a -> XConfig a
ewmh' c = c { startupHook     = startupHook c +++ ewmhDesktopsStartup
            , handleEventHook = handleEventHook c <> myEwmhDesktopsEventHook
            , logHook         = logHook c +++ ewmhDesktopsLogHook
            }
 -- @@@ will fix this correctly later with the rewrite
  where x +++ y = mappend y x

myConfig =
  ewmh'
    $            def
                   { terminal           = myTerminal
                   , modMask            = mod4Mask
                   , startupHook        = myStartupHook
                   , manageHook         = manageSpawn <+> manageHook def
                   , focusedBorderColor = "#fb9224"
                   , normalBorderColor  = "#000"
                   , focusFollowsMouse = True
                   , borderWidth        = 1
                   , logHook            = myEventLogHook
                   , layoutHook         = myLayoutHook
                   , handleEventHook    = handleEventHook def
                                          -- <+> focusOnMouseMove
                                          <+> fullscreenEventHook
                   , keys               = myKeys
                   }
    `removeKeys` myRemovedKeys

myStartupHook = do
  spawnOnOnce "8" "todoist"
  spawnOnOnce "8" "spotify"
  spawnOnOnce "2" "code"
  spawnOnOnce "2" myTerminal
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
