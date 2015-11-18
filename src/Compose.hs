{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleInstances #-}

module Compose
  ( Sound(..)
  , Hit
  , hit
  , tone
  , vol
  , dur
  , Composition(..)
  , emptyC
  , Compose
  , ComposeM
  , cmap
  , execCompose
  , compose
  , strike
  , mkComposition
  , orbit
  , clone

  )where

import Control.Applicative
import Control.Monad
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

data Composition =
    Prim  Hit
  | Chain Composition Composition
  | Par   Composition Composition
  deriving (Show)

emptyC :: Composition
emptyC = Prim (Hit BassDrum1 0 0)

type ComposeM = Compose ()

newtype Compose a = Compose {unCompose :: (Composition, a)}

cmap :: (Hit -> Hit) -> Compose a -> Compose a
cmap f (Compose (c,a)) = Compose $ (hmap f c, a)
  where
    hmap f (Prim h)      = Prim (f h)
    hmap f (Chain c1 c2) = Chain (hmap f c1) (hmap f c2)
    hmap f (Par   c1 c2) = Par   (hmap f c1) (hmap f c2)

instance Functor Compose where
  fmap = liftM

instance Applicative Compose where
  pure  = return
  (<*>) = ap

-- | This is basically a writer monad specialized to accumulating compositions
--   horizontally.
instance Monad Compose where
  return a = Compose (emptyC, a)
  Compose (c, a) >>= k =
    let (Compose (c', a')) = k a
    in  Compose ((Chain c c'), a')

-------------------------------------------------------------------------------

execCompose :: Compose a -> Composition
execCompose = fst . unCompose

compose :: Composition -> ComposeM
compose c = Compose (c, ())

strike :: Hit -> ComposeM
strike = compose . Prim

-- | Play two compositions in parallel.
instance Monoid (Compose ()) where
  mempty        = Compose (emptyC, ())
  mappend c1 c2 = Compose (Par (execCompose c1) (execCompose c2), ())

mkComposition :: [Hit] -> ComposeM
mkComposition hits = compose $ mkComp hits
  where
    mkComp hits = foldr1 Chain (map Prim hits)

orbit :: Compose a -> Compose a
orbit c = c >> orbit c

clone :: Int -> Compose a -> Compose a
clone 1 c = c
clone n c = c >> clone (n-1) c
--------------------------------------------------------------------------------
