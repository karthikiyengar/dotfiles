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

myBar = "polybar xmother"

myStatusBar = statusBar myBar def def

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
  ]


myConfig =
  def { terminal           = "tilix"
      , modMask            = mod4Mask
      , focusedBorderColor = "#008080"
      , logHook            = myEventLogHook
      , layoutHook         = avoidStruts $ layoutHook defaultConfig
      }
    `additionalKeysP` myKeysP
    `additionalKeys`  myKeys


myEventLogHook = do
  winset <- gets windowset
  title  <- maybe (return "") (fmap show . getName) . W.peek $ winset
  let currWs = W.currentTag winset
  let wss    = map W.tag $ W.workspaces winset
  let wsStr  = join $ map (fmt currWs) $ sort' wss

  io $ appendFile "/tmp/.xmonad-title-log" (title ++ "\n")
  io $ appendFile "/tmp/.xmonad-workspace-log" (wsStr ++ "\n")

 where
  fmt currWs ws | currWs == ws = "[" ++ ws ++ "]"
                | otherwise    = " " ++ ws ++ " "
  sort' = sortBy (compare `on` (!! 0))


main = do
  forM_ [".xmonad-workspace-log", ".xmonad-title-log"] $ \file -> do
    safeSpawn "mkfifo" ["/tmp/" ++ file]
  xmonad =<< myStatusBar myConfig




