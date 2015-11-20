{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleInstances #-}

module Compose
  ( Sound(..)
  , Hit
  , hit
  , tone
  , vol
  , dur
  , Command(..)
  , Sequence(..)
  , Song
  , Tempo
  , SequenceR
  , seque
  , song
  , runSequenceR
  , sequeMap
  , execSequence
  , strike
  , orbit
  , clone

  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Data.Monoid
import Control.Lens
-------------------------------------------------------------------------------
-- | The available instruments.
data Sound =
     BassDrum2     | BassDrum1     | SideStick    | SnareDrum1
   | HandClap      | SnareDrum2    | LowTom2      | ClosedHihat
   | LowTom1       | PedalHihat    | MidTom2      | OpenHihat
   | MidTom1       | HighTom2      | CrashCymbal1 | HighTom1
   | RideCymbal1   | ChineseCymbal | RideBell     | Tambourine
   | SplashCymbal  | Cowbell       | CrashCymbal2 | VibraSlap
   | RideCymbal2   | HighBongo     | LowBongo     | MuteHighConga
   | OpenHighConga | LowConga      | HighTimbale  | LowTimbale
   | HighAgogo     | LowAgogo      | Cabasa       | Maracas
   | ShortWhistle  | LongWhistle   | ShortGuiro   | LongGuiro
   | Claves        | HighWoodBlock | LowWoodBlock | MuteCuica
   | OpenCuica     | MuteTriangle  | OpenTriangle
   deriving (Show,Eq,Ord,Enum)

-- | A single drum strike, 'tone' is the instrument, dur the duration, and
--  vol the volume.
data Hit = Hit
    { _tone :: Sound
    , _dur  :: Int
    , _vol  :: Int
    } deriving (Show)

makeLenses ''Hit

-- | Constructor fo 'Hit'.
hit :: Sound -> Int -> Int -> Hit
hit t d v = Hit t d v

-- | The commands the comprixe a song. Prim for a single drum hit. Chain for
--   a sequence of hits. Par for making to sequences run in parallel. And BPM,
--   Vol, for tempo and volume.
data Command =
    Prim  Hit
  | Chain Command Command
  | Par   Command Command
  | None
  deriving (Show)

newtype Sequence a = Sequence {unSequence :: (Command, a)}

type Song = Sequence ()

-- | The tempo in beats per minute, the volume for 0 to 127.
type Tempo = Int

-- Cutom monad ecapsulating a drum machine. Controls such as volume and
-- tempo are the state.
type SequenceR = ReaderT Tempo Sequence

-- | Contert the drum maching into a Sequence to be played
runSequenceR :: Tempo -> SequenceR a -> Sequence a
runSequenceR  = flip runReaderT

-- | Map a function on hits over a song.
sequeMap :: (Hit -> Hit) -> Sequence a -> Sequence a
sequeMap f (Sequence (c,a)) = Sequence $ (hmap f c, a)
  where
    hmap f (Prim h)      = Prim  (f h)
    hmap f (Chain b1 b2) = Chain (hmap f b1) (hmap f b2)
    hmap f (Par   b1 b2) = Par   (hmap f b1) (hmap f b2)
    hmap _ b             = b

instance Functor Sequence where
  fmap = liftM

instance Applicative Sequence where
  pure  = return
  (<*>) = ap

-- | This is basically a writer monad specialized to accumulating Commands
--   horizontally.
instance Monad Sequence where
  return a = Sequence (None, a)
  Sequence (b, a) >>= k =
    let (Sequence (b', a')) = k a
    in  Sequence ((Chain b b'), a')

-------------------------------------------------------------------------------
-- | Unwarp a command.
execSequence :: Sequence a -> Command
execSequence = fst . unSequence

-- | Wrap a command.
seque :: Command -> a -> Sequence a
seque b a = Sequence (b, a)

-- | Convenience function to wrap Commands with a = ().
song :: Command -> Song
song b = seque b ()

-- | Convert a hit to a 'Sequence ()'.
strike :: Hit -> Song
strike h = seque (Prim h) ()

-- | Play two Commands in parallel.
instance Monoid (Song) where
  mempty        = Sequence (None, ())
  mappend b1 b2 = Sequence (Par (execSequence b1) (execSequence b2), ())

-- | Loop a song forever.
orbit :: Sequence a -> Sequence a
orbit b = b >> orbit b

-- | Replicate a song n times.
clone :: Int -> Sequence a -> Sequence a
clone 1 b = b
clone n b = b >> clone (n-1) b
--------------------------------------------------------------------------------
