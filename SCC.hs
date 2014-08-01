{-# LANGUAGE TemplateHaskell #-}
module Main where

import Common
import qualified Data.IntMap as IntMap
import           Data.IntMap (IntMap)
import qualified Data.IntSet as IntSet
import           Data.IntSet (IntSet)
import           Data.List (foldl')
import           Data.Foldable (for_)
import           Data.Monoid
import           Data.Maybe
import           Control.Monad.State

import Control.Lens

data TarjanState = TarjanState
  { _nextIndex :: Int
  , _stack     :: [Int]
  , _indexes   :: IntMap Int
  , _lowlinks  :: IntMap Int
  , _output    :: [[Int]]
  }

makeLenses ''TarjanState


main = do
  g <- readDGraph
  printList [length (tarjan g)]

tarjan :: Graph -> [[Int]]
tarjan g = view output (execState m (TarjanState 0 mempty mempty mempty []))
  where
  m = for_ (IntMap.keys g) $ \v ->
        do i <- use (indexes . at v)
           when (isNothing i) (strongConnect v)

  strongConnect v =
    do -- Set the depth index for v to the smallest unused index
       i <- nextIndex <<+= 1

       indexes  . at v ?= i
       lowlinks . at v ?= i
       stack           %= cons v

       -- Consider successors of v
       forOf_ (ix v . folded) g $ \w ->
         do mbWix <- use (indexes . at w)
            case mbWix of
              Nothing ->
                do strongConnect w
                   Just vlow <- use (lowlinks . at v)
                   Just wlow <- use (lowlinks . at w)
                   lowlinks . at v ?= min vlow wlow
              Just wix ->
                do wInS <- uses stack (elem w)
                   when wInS $
                     do Just vlow <- use (lowlinks . at v)
                        lowlinks . at v ?= min vlow wix

       Just vlow <- use (lowlinks . at v)
       Just vix  <- use (indexes  . at v)
       when (vlow == vix) $
         do --start
            let loop acc =
                   do w:ws <- use stack
                      stack .= ws
                      if w/=v then loop (w:acc) else output %= cons (w:acc)
            loop []
