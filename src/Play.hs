--------------------------------------------------------------------------------
-- |
-- Module      :  Play
-- Author      : Jeffrey Rosenbluth
-- Copyright   : (c) 2015, Jeffrey Rosenbluth
--
-- Interface to synth.
-- Mostly copied from: https://github.com/5outh/Bang
--    Copyright (c) 2014 Benjamin Kovach
-------------------------------------------------------------------------------

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

import Core
import Drum
import Interpret
import Data.Ratio

hitToMidiEvent :: Hit -> MidiEvent
hitToMidiEvent h = MidiEvent d (MidiMessage 1 (NoteOn t v))
  where
    t = 35 + fromEnum (h ^. tone)
    d = fromRatio $ h ^. duration
    v = fromRatio $ h ^. velocity
    fromRatio r = fromIntegral $ numerator r `div` denominator r

getConnection :: IO Connection
getConnection = do
    dstlist <- enumerateDestinations
    case dstlist of
      [] -> error "No MIDI Devices found."
      (dst:_) -> openDestination dst

-- | Play a song given tempo in beats per minute and volume for 0 to 127.
play' :: Beat a -> Rational -> IO ()
play' s b = do
    conn <- getConnection
    start conn
    evalStateT runComposition (conn, interpret (Control b 1 1) s)
    close conn

-- | Play a song with tempo and volume set to defaults
play :: Beat a -> IO ()
play s = play' s 120

-- | Loop a song forever, given tempo in beats per minute and
--   volume for 0 to 127.
loop' :: Beat a -> Rational -> IO ()
loop' s b = do
  conn <- getConnection
  start conn
  evalStateT runComposition (conn, interpret (Control b 1 1) $ orbit s)
  close conn

-- | Loop a song with volume and tempo set to defaults.
loop :: Beat a -> IO ()
loop s = loop' s 120

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
