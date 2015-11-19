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
  , Song(..)
  , Song'
  , Control(..)
  , tempo
  , volume
  , DrumMachine
  , song
  , song'
  , bpm
  , level
  , runDrumMachine
  , songMap
  , execSong
  , strike
  , mkCommand
  , orbit
  , clone

  )where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Monoid
import Control.Lens
-------------------------------------------------------------------------------

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

data Hit = Hit
    { _tone :: Sound
    , _dur  :: Int
    , _vol  :: Int
    } deriving (Show)

makeLenses ''Hit

hit :: Sound -> Int -> Int -> Hit
hit t d v = Hit t d v

data Command =
    Prim  Hit
  | Chain Command Command
  | Par   Command Command
  | BPM   Int
  | Vol   Int
  | None
  deriving (Show)

-- type Song = Song ()

newtype Song a = Song {unSong :: (Command, a)}

type Song' = Song ()

type Tempo = Int

data Control = Control
  { _tempo  :: Int
  , _volume :: Int
  }

makeLenses '' Control

type DrumMachine = StateT Control Song

runDrumMachine :: Control -> DrumMachine a -> Song a
runDrumMachine  = flip evalStateT

songMap :: (Hit -> Hit) -> Song a -> Song a
songMap f (Song (c,a)) = Song $ (hmap f c, a)
  where
    hmap f (Prim h)      = Prim  (f h)
    hmap f (Chain b1 b2) = Chain (hmap f b1) (hmap f b2)
    hmap f (Par   b1 b2) = Par   (hmap f b1) (hmap f b2)
    hmap _ b             = b

instance Functor Song where
  fmap = liftM

instance Applicative Song where
  pure  = return
  (<*>) = ap

-- | This is basically a writer monad specialized to accumulating Commands
--   horizontally.
instance Monad Song where
  return a = Song (None, a)
  Song (b, a) >>= k =
    let (Song (b', a')) = k a
    in  Song ((Chain b b'), a')

-------------------------------------------------------------------------------

execSong :: Song a -> Command
execSong = fst . unSong

song :: Command -> a -> Song a
song b a = Song (b, a)

song' :: Command -> Song'
song' b = song b ()

strike :: Hit -> Song'
strike h = song (Prim h) ()

bpm :: Int -> Song'
bpm = song' . BPM

level :: Int -> Song'
level = song' . Vol

-- | Play two Commands in parallel.
instance Monoid (Song') where
  mempty        = Song (None, ())
  mappend b1 b2 = Song (Par (execSong b1) (execSong b2), ())

mkCommand :: [Hit] -> Song'
mkCommand hits = song (mkComp hits) ()
  where
    mkComp hits = foldr1 Chain (map Prim hits)

orbit :: Song a -> Song a
orbit b = b >> orbit b

clone :: Int -> Song a -> Song a
clone 1 b = b
clone n b = b >> clone (n-1) b
--------------------------------------------------------------------------------
