{-# LANGUAGE OverloadedStrings #-}

import           XMonad                  hiding ( (|||) )
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.Grid
import           XMonad.Layout.NoBorders
import qualified XMonad.Prompt                 as P
import qualified XMonad.Actions.Submap         as SM
import qualified XMonad.Actions.Search         as S
import           XMonad.Hooks.FadeInactive             ( fadeInactiveLogHook )
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.DwmStyle
import           Data.List                      ( sortBy )
import           Data.Function                  ( on )
import           Control.Monad                  ( forM_
                                                , join
                                                )
import           XMonad.Util.Run                ( safeSpawn, hPutStrLn )
import           XMonad.Util.NamedWindows       ( getName )
import           XMonad.Hooks.DynamicLog
import qualified XMonad.StackSet               as W
import           XMonad.Hooks.EwmhDesktops      ( ewmh
                                                )
import XMonad.Hooks.ManageDocks
    ( docks,
      ToggleStruts(ToggleStruts),
      avoidStruts,
    )
import           XMonad.Util.EZConfig
import           Graphics.X11.ExtraTypes.XF86
import           XMonad.Hooks.SetWMName
import           XMonad.Actions.SpawnOn
import           XMonad.Util.SpawnOnce
import qualified Data.Map                      as M
import           XMonad.Actions.CopyWindow      ( copy )
import           Data.Text                      ( pack, replace, unpack )
import           System.Taffybar.Support.PagerHints (pagerHints)

import qualified Codec.Binary.UTF8.String as UTF8



myTerminal = "alacritty"
myMod = mod4Mask -- Super Key
altMask = mod1Mask -- Alt Key
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]


searchEngineMap method =
  M.fromList [ ((0, xK_a), method S.alpha)
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
  [ ((modm .|. altMask, xK_l), spawn "slock")
    ]

    ++ [ ( (modm, xK_p)
         , spawn
           "rofi -combi-modi window,drun,run,calc -theme gruvbox-dark-soft -show combi -modi combi,calc,run -terse -no-show-match -no-sort -location 1 -width 100 -show-icons -font 'Hasklig Regular 12'  -calc-command \"echo '{result}' | xsel -b\""
         )
       , ((modm, xK_v), spawn "pavucontrol -t 3")
       , ((modm .|. shiftMask, xK_b), spawn "~/.wm-scripts/toggle-bluetooth.sh")
       , ((modm .|. altMask, xK_b), spawn "blueman-manager")
       , ((modm, xK_i), spawn "rofi -show emoji -modi emoji -theme gruvbox-dark-soft -location 1 -width 100")
       , ( (modm, xK_u)
         , spawn
           "unipicker --copy --command 'rofi -dmenu -theme gruvbox-dark-soft -location 1 -width 100'"
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
           "playerctl pause"
         )
       , ( (0, xF86XK_AudioPlay)
         , spawn
           "playerctl play-pause"
         )
       , ( (0, xF86XK_AudioNext)
         , spawn
           "playerctl next"
         )
       , ( (0, xF86XK_AudioPrev)
         , spawn
           "playerctl previous"
         )
       , ( (0, xF86XK_MonBrightnessUp)
         , spawn "~/.wm-scripts/media.sh brightness-inc"
         ) -- Added to visudo, deb manually installed
       , ( (0, xF86XK_MonBrightnessDown)
         , spawn "~/.wm-scripts/media.sh brightness-dec"
         )
       , ((0, xK_Print), spawn "flameshot gui")

       , ((modm, xK_backslash), spawn "playerctl --player=spotify play-pause")
       , ((modm, xK_bracketleft), spawn "playerctl --player=spotify previous")
       , ((modm, xK_bracketright), spawn "playerctl --player=spotify next")
       , ((modm, xK_f), spawn "~/.wm-scripts/zzzfoo -n 0 -o xdg-open -r '-theme gruvbox-dark-soft -location 1 -width 100 -height 50'")
       , ((modm .|. shiftMask, xK_f), spawn "caja")
       , ( (modm .|. shiftMask, xK_h)
         , spawn
           "rofi -modi 'clipboard:greenclip print' -theme solarized -show clipboard -terse -no-show-match -no-sort -location 1 -width 100 -run-command '{cmd}'"
         )
       , ( (modm, xK_a)
         , sequence_ $ [ windows $ copy i | i <- XMonad.workspaces conf ]
         )    -- Pin to all workspaces
       , ( (modm .|. shiftMask, xK_a)
         , windows kill8
         )    -- remove from all but current
       , ((modm, xK_s), SM.submap $ searchEngineMap $ S.promptSearch P.def)
       , ( (modm .|. shiftMask, xK_s)
         , SM.submap $ searchEngineMap S.selectSearch
         )
       , ((modm, xK_b), sendMessage ToggleStruts) -- toggle the statusbar gap
       ]
    -- Screen order for triple screens.
    ++ [ ( (m .|. modm, key)
         , screenWorkspace sc >>= flip whenJust (windows . f)
         )
       | (key, sc) <- zip [xK_w, xK_e, xK_r] [0, 1, 2]
       , (f  , m ) <- [(W.greedyView, 0), (W.shift, shiftMask)]
       ]

myRemovedKeys = [(myMod .|. shiftMask, xK_q)]


data AllFloats = AllFloats deriving (Read, Show)
instance SetsAmbiguous AllFloats where
    hiddens _ wset _ _ _ = M.keys $ W.floating wset

myLayoutHook =
    avoidStruts
    -- $ lessBorders AllFloats -- To remove borders for floating windows
    $ smartBorders
    $ dwmStyle shrinkText def (   (Tall 1 (3 / 100) (1 / 2))
      ||| Grid
      ||| ThreeCol 1 (3 / 100) (1 / 3)
      ||| Full
      )

-- https://wiki.haskell.org/Xmonad/Frequently_asked_questions#I_need_to_find_the_class_title_or_some_other_X_property_of_my_program
-- TLDR; xprop _NET_WM_CLASS (second property)
myManageHook = composeAll
  [ title =? "Emulator" --> doFloat
  , title =? "Android Emulator - pixel:5554" --> doFloat
  , className =? "Code" --> doShift "2"
  , className =? "Firefox" --> doShift "1"
  , className =? "Spotify" --> doShift "8"
  , className =? "Chromium-browser" --> doShift "4"
  ]

myStartupHook = do
  spawnOnce "~/.wm-scripts/startup.sh"
  spawn "killall taffybar; taffybar &!"
  spawnOnOnce "8" "spotify"
  spawnOnOnce "1" "firefox"
  spawnOnOnce "9" "thunderbird"
  setWMName "LG3D"


kill8 ss | Just w <- W.peek ss = W.insertUp w $ W.delete w ss
         | otherwise           = ss

myLogHook = fadeInactiveLogHook 0.9

main :: IO ()
main  = xmonad . ewmh $ docks $ pagerHints $ def { terminal           = myTerminal
          , modMask            = myMod
          , startupHook        = myStartupHook
          , manageHook         = manageSpawn <+> myManageHook <+> manageHook def
          , focusedBorderColor = "#fb9224"
          , normalBorderColor  = "#000"
          , borderWidth        = 3
          , layoutHook         = myLayoutHook
          , keys               = myKeys
          }
    `removeKeys` myRemovedKeys

-- TODO: spawnOnOnce steals focus at startup
