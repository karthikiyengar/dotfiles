-- Reference for onClick - https://github.com/frioux/dotfiles/blob/e9a710fd417a2eb8a0989cba700693001ca57977/taffybar/taffybar.hs
-- https://github.com/taffybar/taffybar/issues/196
-- https://github.com/frioux/dotfiles/commit/c6bcb5bcefe7087fcc98f251a58a0ce74ce55550
-- https://github.com/taffybar/gtk-sni-tray/blob/07a8b24db5245b58ca39ed6c2fd6bac4e51ccc13/src/StatusNotifier/Tray.hs#L340
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Example
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
module Main where

-- XXX: in an actual taffybar.hs configuration file, you will need the module
-- name to be Main, and you would need to have a main function defined at the
-- top level, e.g.
--
-- > main = dyreTaffybar exampleTaffybarConfig

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified Data.Text as T
import GI.Gdk (EventButton)
import qualified GI.Gtk as Gtk
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process
import System.Taffybar
import System.Taffybar.Context (Context, TaffybarConfig (..))
import System.Taffybar.Hooks (withBatteryRefresh, withLogServer)
import System.Taffybar.Information.CPU
import System.Taffybar.Information.Memory
  ( MemoryInfo (memoryUsedRatio),
    parseMeminfo,
  )
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.PollingGraph
  ( GraphConfig
      ( graphBackgroundColor,
        graphBorderWidth,
        graphDataColors,
        graphLabel,
        graphPadding,
        graphWidth
      ),
    defaultGraphConfig,
    pollingGraphNew,
  )
import System.Taffybar.Widget.Generic.PollingLabel (pollingLabelNew)

transparent,
  yellow1,
  yellow2,
  green1,
  green2,
  taffyBlue ::
    (Double, Double, Double, Double)
transparent = (0.0, 0.0, 0.0, 0.0)
yellow1 = (0.9453125, 0.63671875, 0.2109375, 1.0)
yellow2 = (0.9921875, 0.796875, 0.32421875, 1.0)
green1 = (0, 1, 0, 1)
green2 = (1, 0, 1, 0.5)
taffyBlue = (0.129, 0.588, 0.953, 1)

myGraphConfig, netCfg, memCfg, cpuCfg :: GraphConfig
myGraphConfig =
  defaultGraphConfig
    { graphPadding = 0,
      graphBorderWidth = 0,
      graphWidth = 75,
      graphBackgroundColor = transparent
    }
netCfg =
  myGraphConfig
    { graphDataColors = [yellow1, yellow2],
      graphLabel = Just "net"
    }
memCfg =
  myGraphConfig
    { graphDataColors = [taffyBlue],
      graphLabel = Just "mem"
    }
cpuCfg =
  myGraphConfig
    { graphDataColors = [green1, green2],
      graphLabel = Just "cpu"
    }

wcfg = (defaultWeatherConfig "KMSN") {weatherTemplate = "$tempC$ C @ $humidity$"}

memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback :: IO [Double]
cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

try' :: IO a -> IO (Either IOException a)
try' = try

caffCallback :: EventButton -> IO Bool
caffCallback event = do
  putStrLn "Called"
  result <-
    try' $
      createProcess
        (proc "./caffeinate-toggle.sh" [])
          { cwd = Just "/home/kiyengar/.config/polybar/scripts"
          }
  case result of
    Left ex -> putStrLn $ "error starting: " ++ show ex
    Right (_, _, _, p) -> putStrLn "started ok"
  return True

caff ::
  MonadIO m =>
  -- | Polling interval (in seconds, e.g. 500)
  Double ->
  m Gtk.Widget
caff interval = liftIO $ do
  label <- pollingLabelNew interval showFSInfo
  ebox <- Gtk.eventBoxNew
  Gtk.containerAdd ebox label
  Gtk.eventBoxSetVisibleWindow ebox False
  _ <- Gtk.onWidgetButtonPressEvent ebox caffCallback
  Gtk.widgetShowAll ebox
  Gtk.toWidget ebox

getLabel :: ExitCode -> String
getLabel exitCode =
  case exitCode of
    ExitSuccess -> "💤"
    ExitFailure x -> "☕"

showFSInfo :: IO T.Text
showFSInfo = do
  (exitCode, fsOut, _) <- readProcessWithExitCode "pgrep" ["-x", "xidlehook"] ""
  let fss = unwords $ take 1 $ lines fsOut
      label = words $ getLabel exitCode
  return $ T.pack $ unwords label

main = do
  let myWorkspacesConfig =
        defaultWorkspacesConfig
          { minIcons = 1,
            widgetGap = 0,
            showWorkspaceFn = hideEmpty
          }
      workspaces = workspacesNew myWorkspacesConfig
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      mem = pollingGraphNew memCfg 1 memCallback
      net = networkGraphNew netCfg Nothing
      clock = textClockNewWith defaultClockConfig
      layout = layoutNew defaultLayoutConfig
      windowsW = windowsNew defaultWindowsConfig
      -- See https://github.com/taffybar/gtk-sni-tray#statusnotifierwatcher
      -- for a better way to set up the sni tray
      tray = sniTrayNew
      myConfig =
        defaultSimpleTaffyConfig
          { startWidgets =
              workspaces : map (>>= buildContentsBox) [layout, windowsW],
            endWidgets =
              map
                (>>= buildContentsBox)
                [ batteryIconNew,
                  caff 1,
                  clock,
                  tray,
                  cpu,
                  mem,
                  net,
                  mpris2New
                ],
            barPosition = Top,
            barPadding = 0,
            barHeight = 40,
            widgetSpacing = 0
          }
  startTaffybar $ withBatteryRefresh $ withLogServer $ toTaffyConfig myConfig