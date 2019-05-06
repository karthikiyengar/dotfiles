import           XMonad
import           Data.List                      ( sortBy )
import           Data.Function                  ( on )
import           Control.Monad                  ( forM_
                                                , join
                                                )
import           XMonad.Util.Run                ( safeSpawn )
import           XMonad.Util.NamedWindows       ( getName )
import           XMonad.Hooks.DynamicLog
import qualified XMonad.StackSet               as W
import           System.IO
import           XMonad.Hooks.EwmhDesktops      ( ewmh )
import           XMonad.Hooks.ManageDocks
import           XMonad.Util.EZConfig
import           Graphics.X11.ExtraTypes.XF86
import           XMonad.Actions.PhysicalScreens

myBar = "killall -q polybar; polybar xmother"

myStatusBar = statusBar myBar (PP { ppOutput = \s -> return () }) def

myMod = mod4Mask

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

myKeysP =
  [ (otherModMasks ++ "M-" ++ [key], action tag)
  | (tag          , key   ) <- zip myWorkspaces "123456789"
  , (otherModMasks, action) <-
    [ ( ""
      , windows . W.view
      ) -- was W.greedyView
    , ("S-", windows . W.shift)
    ]
  ]

myKeys =
  [ ( (myMod, xK_b)
    , sequence_ [spawn "polybar-msg cmd toggle", sendMessage ToggleStruts]
    )
    , ((myMod .|. shiftMask, xK_l), spawn "xscreensaver-command -lock")
    ]

    ++ [ ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume 0 +5%")
       , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume 0 -5%")
       , ((0, xF86XK_AudioMute)       , spawn "pactl set-sink-mute 0 toggle")
       ]

    -- Screen order for triple screens. TODO: Refactor
    ++ [ ((myMod, xK_w)              , viewScreen 2)
       , ((myMod, xK_e)              , viewScreen 0)
       , ((myMod, xK_r)              , viewScreen 1)
       , ((myMod .|. shiftMask, xK_w), sendToScreen 2)
       , ((myMod .|. shiftMask, xK_e), sendToScreen 0)
       , ((myMod .|. shiftMask, xK_r), sendToScreen 1)
       ]

---- amixer -D pulse sset Master 5%- > /dev/null

myConfig =
  def { terminal           = "termite"
      , modMask            = mod4Mask
      , focusedBorderColor = "#008080"
      , borderWidth        = 2
      , layoutHook         = avoidStruts $ layoutHook def
      }
    `additionalKeysP` myKeysP
    `additionalKeys`  myKeys

main = do
  forM_ [".xmonad-workspace-log", ".xmonad-title-log"] $ \file -> do
    safeSpawn "mkfifo" ["/tmp/" ++ file]
  xmonad =<< myStatusBar myConfig




