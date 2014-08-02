{-# LANGUAGE TypeFamilies #-}
module Neg where

import Data.Profunctor
import Data.Monoid
import Control.Category
import Prelude hiding (id, (.))


newtype Neg r a b = Neg { runNeg :: (b -> r) -> (a -> r) }

instance Functor (Neg r a) where
  fmap cb (Neg crar) = Neg $ \br -> crar (br.cb)

instance Profunctor (Neg r) where
  rmap = fmap
  lmap ab (Neg crbr) = Neg $ \cr a -> crbr cr (ab a)

neg :: (a -> b) -> Neg r a b
neg ba = Neg (\ar b -> ar (ba b))

mapResult :: (r -> r) -> Neg r a a
mapResult f = Neg (\ar a -> f (ar a))

branch :: (a -> Neg r a b) -> Neg r a b
branch f = Neg (\ar a -> runNeg (f a) ar a)

instance Category (Neg r) where
  id = Neg (\ar a -> ar a)
  Neg f . Neg g = Neg (\cr a -> g (f cr) a)

instance a ~ b => Monoid (Neg r a b) where
  mempty = id
  mappend = flip (.)
