{-# LANGUAGE TypeFamilies #-}
module Neg where

import Data.Profunctor
import Data.Profunctor.Rep
import Data.Monoid
import Control.Category
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
