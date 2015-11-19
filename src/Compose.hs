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
    hmap f (Prim h)      = Prim (f h)
    hmap f (Chain c1 c2) = Chain (hmap f c1) (hmap f c2)
    hmap f (Par   c1 c2) = Par   (hmap f c1) (hmap f c2)

instance Functor SongM where
  fmap = liftM

instance Applicative SongM where
  pure  = return
  (<*>) = ap

-- | This is basically a writer monad specialized to accumulating Beats
--   horizontally.
instance Monad SongM where
  return a = SongM (None, a)
  SongM (c, a) >>= k =
    let (SongM (c', a')) = k a
    in  SongM ((Chain c c'), a')

-------------------------------------------------------------------------------

execSongM :: SongM a -> Beat
execSongM = fst . unSongM

songM :: Beat -> a -> SongM a
songM b a = SongM (b, a)

song :: Beat -> Song
song c = SongM (c, ())

strike :: Hit -> Song
strike = song . Prim

-- | Play two Beats in parallel.
instance Monoid (SongM ()) where
  mempty        = SongM (None, ())
  mappend c1 c2 = SongM (Par (execSongM c1) (execSongM c2), ())

mkBeat :: [Hit] -> Song
mkBeat hits = song $ mkComp hits
  where
    mkComp hits = foldr1 Chain (map Prim hits)

orbit :: SongM a -> SongM a
orbit c = c >> orbit c

clone :: Int -> SongM a -> SongM a
clone 1 c = c
clone n c = c >> clone (n-1) c
--------------------------------------------------------------------------------
