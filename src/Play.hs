module Play
  ( play
  , loop
  )
where

import Control.Monad
import Control.Monad.State
import Control.Lens
import Control.Concurrent
import System.MIDI
import System.Info

import Compose
import Interpret

hitToMidiEvent :: Hit -> MidiEvent
hitToMidiEvent h = MidiEvent d (MidiMessage 1 (NoteOn t v))
  where
    t = 35 + fromEnum (h ^. tone)
    d = fromIntegral $ h ^. dur
    v = h ^. vol

getConnection :: IO Connection
getConnection = do
    dstlist <- enumerateDestinations
    case dstlist of
      [] -> error "No MIDI Devices found."
      (dst:_) -> openDestination dst

play :: Riff -> IO ()
play comp = do
    conn <- getConnection
    start conn
    evalStateT runComposition (conn, interpret comp)
    close conn

loop :: Compose a -> IO ()
loop comp = do
  conn <- getConnection
  start conn
  evalStateT runComposition (conn, interpret $ orbit comp)
  close conn

runComposition :: StateT (Connection, [Hit]) IO ()
runComposition = do
  (conn, hits) <- get
  t <- lift $ currentTime conn
  case hits of
    []     -> return ()
    (h:hs) -> do
      let x@(MidiEvent s ev) = hitToMidiEvent h
      when (s < t) $ do
        put (conn, hs)
        lift $ send conn ev
      lift $ threadDelay 250
      runComposition
