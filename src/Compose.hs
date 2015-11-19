{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleInstances #-}

module Compose
  ( Sound(..)
  , Hit
  , hit
  , tone
  , vol
  , dur
  , Beat(..)
  , SongM(..)
  , Song
  , SongMonad
  , songM
  , song
  , Tempo
  , runSongMonad
  , songMap
  , execSongM
  , strike
  , mkBeat
  , orbit
  , clone

  )where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
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

data Beat =
    Prim  Hit
  | Chain Beat Beat
  | Par   Beat Beat
  | BPM
  | None
  deriving (Show)

type Song = SongM ()

newtype SongM a = SongM {unSongM :: (Beat, a)}

type Tempo = Int

type SongMonad = ReaderT Tempo SongM

runSongMonad :: Tempo -> SongMonad a -> SongM a
runSongMonad  = flip runReaderT

songMap :: (Hit -> Hit) -> SongM a -> SongM a
songMap f (SongM (c,a)) = SongM $ (hmap f c, a)
  where
    hmap f (Prim h)      = Prim  (f h)
    hmap f (Chain b1 b2) = Chain (hmap f b1) (hmap f b2)
    hmap f (Par   b1 b2) = Par   (hmap f b1) (hmap f b2)
    hmap _ b             = b

instance Functor SongM where
  fmap = liftM

instance Applicative SongM where
  pure  = return
  (<*>) = ap

-- | This is basically a writer monad specialized to accumulating Beats
--   horizontally.
instance Monad SongM where
  return a = SongM (None, a)
  SongM (b, a) >>= k =
    let (SongM (b', a')) = k a
    in  SongM ((Chain b b'), a')

-------------------------------------------------------------------------------

execSongM :: SongM a -> Beat
execSongM = fst . unSongM

songM :: Beat -> a -> SongM a
songM b a = SongM (b, a)

song :: Beat -> Song
song b = SongM (b, ())

strike :: Hit -> Song
strike = song . Prim

-- | Play two Beats in parallel.
instance Monoid (SongM ()) where
  mempty        = SongM (None, ())
  mappend b1 b2 = SongM (Par (execSongM b1) (execSongM b2), ())

mkBeat :: [Hit] -> Song
mkBeat hits = song $ mkComp hits
  where
    mkComp hits = foldr1 Chain (map Prim hits)

orbit :: SongM a -> SongM a
orbit b = b >> orbit b

clone :: Int -> SongM a -> SongM a
clone 1 b = b
clone n b = b >> clone (n-1) b
--------------------------------------------------------------------------------
