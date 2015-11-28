--------------------------------------------------------------------------------
-- |
-- Module      :  Interpret
-- Author      : Jeffrey Rosenbluth
-- Copyright   : (c) 2015, Jeffrey Rosenbluth
--
-- Convert 'drum' to Midi.
--
-------------------------------------------------------------------------------

module Interpret
  ( interpret
  ) where

import Core
import Control.Lens
import Control.Monad.Reader
import Data.Ratio
import System.MIDI

-------------------------------------------------------------------------------
-- | Prepare a song and it's applyControls to be played.
interpret :: Control -> Beat a -> [MidiEvent]
interpret t = toMidiEvents . runSequenceR t . applyControl

merge :: [MidiEvent] -> [MidiEvent] -> [MidiEvent]
merge [] ys = ys
merge xs [] = xs
merge (x@(MidiEvent dx _) : xs) (y@(MidiEvent dy _ ) : ys)
  | dx <= dy  = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

toMidiEvents :: Beat a -> [MidiEvent]
toMidiEvents beat = go 0 (execBeat beat)
  where
    go d (Prim h) = [MidiEvent d' (MidiMessage 1 (NoteOn t v))]
      where
        d' = fromRatio d
        t = 35 + fromEnum (h ^. tone)
        v = fromRatio (h ^. velocity)
    go d (Chain b1 b2) = go d b1 ++ go (d + totalDur b1) b2
    go d (Par   b1 b2) = go d b1 `merge` go d b2
    go _ _             = []
    fromRatio r = fromIntegral $ numerator r `div` denominator r

applyControl :: Beat a -> SequenceR a
applyControl sm = go $ unBeat sm
  where
    go (Prim h, a) = do
      d <- ask
      let m = d ^. bpm * d ^. tempo
          v = h ^. velocity * d ^. level
          v' = max 0 (min v 127)
      lift $ beat (Prim (h & duration *~ (60000 / m)
                           & velocity .~  v)) a
    go (Chain b1 b2, a) = do
      go (b1, a)
      go (b2, a)
    go (Par b1 b2, a) = do
      d <- ask
      let (Beat (t1, b)) = runSequenceR d (go (b1, a))
      let (Beat (t2, c)) = runSequenceR d (go (b2, a))
      lift $ beat (Par t1 t2) c
    go (Tempo x b, a) = do
      local (\c -> c & tempo *~ x) (go (b, a))
    go (Level x b, a) =
      local (\c -> c & level *~ x) (go (b, a))
    go (None, a) = return a
