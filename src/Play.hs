module Play
  ( play
  , play'
  , loop
  , loop'
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

-- | Play a song given tempo in beats per minute and volume for 0 to 127.
play :: Sequence a -> Int -> Int -> IO ()
play s t v = do
    let c = Control t v
    conn <- getConnection
    start conn
    evalStateT runComposition (conn, interpret c s)
    close conn

-- | Play a song with tempo and volume set to defaults
play' :: Sequence a -> IO ()
play' s = play s 120 100

-- | Loop a song forever, given tempo in beats per minute and
--   volume for 0 to 127.
loop :: Sequence a -> Int -> Int -> IO ()
loop s t v = do
  let c = Control t v
  conn <- getConnection
  start conn
  evalStateT runComposition (conn, interpret c $ orbit s)
  close conn

-- | Loop a song with volume and tempo set to defaults.
loop' :: Sequence a -> IO ()
loop' s = loop s 120 100

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
