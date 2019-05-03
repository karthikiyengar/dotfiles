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

myBar = 
 "polybar xmother"

main = do
  forM_ [".xmonad-workspace-log", ".xmonad-title-log"] $ \file -> do
    safeSpawn "mkfifo" ["/tmp/" ++ file]
    xmonad =<< statusBar
      myBar
      def
      def
      def { terminal = "tilix", modMask = mod4Mask, logHook = eventLogHook, focusedBorderColor = "#008080" }



eventLogHook = do
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
