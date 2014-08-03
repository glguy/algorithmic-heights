{-# LANGUAGE TypeFamilies #-}
module Neg where

import Data.Profunctor
import Data.Profunctor.Rep
import Data.Monoid
import Control.Category
import Control.Applicative
import Prelude hiding (id, (.))

import Control.Monad.Trans.Cont

import Data.Void

newtype Neg r a b = Neg { runNeg :: (b -> r) -> (a -> r) }

instance Functor (Neg r a) where
  fmap cb (Neg crar) = Neg $ \br -> crar (br.cb)

instance Profunctor (Neg r) where
  rmap = fmap
  lmap ab (Neg crbr) = Neg $ \cr a -> crbr cr (ab a)

mapResult :: (r -> r) -> Neg r a a
mapResult f = Neg (\ar a -> f (ar a))

abort :: r -> Neg r a b
abort r = Neg (\_ _ -> r)

instance Category (Neg r) where
  id = Neg (\ar a -> ar a)
  Neg f . Neg g = Neg (\cr a -> g (f cr) a)

instance a ~ b => Monoid (Neg r a b) where
  mempty = id
  mappend = flip (.)

instance Representable (Neg r) where
  type Rep (Neg r) = Cont r
  tabulate (d_crr) = Neg (\cr d -> runCont (d_crr d) cr)
  rep (Neg crdr) d = cont (\cr -> crdr cr d)

instance Choice (Neg r) where
  left' (Neg brar) = Neg (\ebcr -> either (brar (ebcr . Left)) (ebcr . Right))

instance Strong (Neg r) where
  first' (Neg brar) = Neg (\bcr (a,c) -> brar (\b -> bcr (b,c)) a)

exec :: Neg r () Void -> r
exec (Neg f) = f absurd ()

instance Applicative (Neg r a) where
  Neg f <*> Neg g = Neg (\h x -> f (\ab_r -> g (\ar -> h (ab_r ar)) x) x)
  pure a = Neg (\f _c -> f a)

instance Monad (Neg r a) where
  return = pure
  Neg f >>= g = Neg (\h a -> f (\b -> runNeg (g b) h a) a)
