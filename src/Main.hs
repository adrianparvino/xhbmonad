module Main where

import Graphics.XHB

import Control.Monad
import Data.IORef
import Data.List
import Data.Word
import Data.Bits

type Dimensions = (Word16, Word16)
type Position = (Word16, Word16)

getDimensions :: SCREEN -> Dimensions
  hold [] mr
getDimensions = (,) <$> width_in_pixels_SCREEN <*> height_in_pixels_SCREEN

positionWindows :: Connection -> [WINDOW] -> IO ()
positionWindows conn (master:windows) = do
  let (w,h) = getDimensions . head . roots_Setup . connectionSetup $ conn
  positionWindow (0,0) (w`div`2, h) conn master
  let h' = h `div` (fromIntegral . length $ windows)
  zipWithM_ (\i -> positionWindow (w`div`2,h'*i) (w`div`2, h') conn) [0..] windows

positionWindow :: Position -> Dimensions -> Connection -> WINDOW -> IO ()
positionWindow (x, y) (w, h) conn window =
  configureWindow conn window $ toValueParam [ (ConfigWindowX     , fromIntegral x)
                                             , (ConfigWindowY     , fromIntegral y)
                                             , (ConfigWindowWidth , fromIntegral w)
                                             , (ConfigWindowHeight, fromIntegral h) ]
handleMapRequest :: IORef [WINDOW] -> Connection -> MapRequestEvent -> IO ()
handleMapRequest windows conn (MkMapRequestEvent _ window) = do
  modifyIORef windows (window:)
  mapWindow conn window
  readIORef windows >>= positionWindows conn
  return ()

handleDestroyNotify :: IORef [WINDOW] -> Connection -> DestroyNotifyEvent -> IO ()
handleDestroyNotify windows conn (MkDestroyNotifyEvent _ window) = do
  modifyIORef windows (delete window)
  return ()

main :: IO ()
main = do
  windows <- newIORef []

  conn <- connect
  case conn of
    Just conn' -> do
      changeWindowAttributes conn' (root_SCREEN . head . roots_Setup . connectionSetup $ conn') $
        toValueParam [(CWEventMask, toMask [EventMaskSubstructureNotify, EventMaskSubstructureRedirect])]
      let setup = connectionSetup conn'
      forever $ do
        ev <- pollForEvent conn'
        case ev of
          Just ev -> do
            catchEvent ev $ handleMapRequest windows conn'
            catchEvent ev $ handleDestroyNotify windows conn'
          Nothing -> pure ()
      pure ()
    Nothing -> error "Connection Failure"
  pure ()

catchEvent :: (Event a, Monad m) => SomeEvent -> (a -> m ()) -> m ()
catchEvent ev f = maybe (return ()) f $ fromEvent ev
