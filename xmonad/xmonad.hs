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
import           XMonad.Hooks.SetWMName

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

-- Use this to detect keys
-- xev | grep -A2 --line-buffered '^KeyRelease' | sed -n '/keycode /s/^.*keycode \([0-9]*\).* (.*, \(.*\)).*$/\1 \2/p'
myKeys =
  [ ( (myMod, xK_b)
    , sequence_ [spawn "polybar-msg cmd toggle", sendMessage ToggleStruts]
    )
    , ((myMod .|. shiftMask, xK_l), spawn "xscreensaver-command -lock")
    ]

    ++ [ ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume 0 +5%")
       , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume 0 -5%")
       , ((0, xF86XK_AudioMute)       , spawn "pactl set-sink-mute 0 toggle")
       , ((myMod, xK_Print), spawn "sh ~/.xmonad/scripts/select-screenshot.sh")
       ]

    -- Screen order for triple screens. TODO: Refactor
    ++ [ ((myMod, xK_w)              , viewScreen 2)
       , ((myMod, xK_e)              , viewScreen 0)
       , ((myMod, xK_r)              , viewScreen 1)
       , ((myMod .|. shiftMask, xK_w), sendToScreen 2)
       , ((myMod .|. shiftMask, xK_e), sendToScreen 0)
       , ((myMod .|. shiftMask, xK_r), sendToScreen 1)
       ]

myRemovedKeys = [((myMod .|. shiftMask, xK_q))]
---- amixer -D pulse sset Master 5%- > /dev/null

myConfig =
  def { terminal           = "termite"
      , modMask            = mod4Mask
      , startupHook        = setWMName "LG3D"
      , focusedBorderColor = "#008080"
      , borderWidth        = 2
      , logHook            = myEventLogHook
      , layoutHook         = avoidStruts $ layoutHook def
      }
    `additionalKeysP` myKeysP
    `additionalKeys`  myKeys
    `removeKeys`      myRemovedKeys

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



