--
--
-- DISCLAIMER: This code is exploring a technique and is not an endorsement :)
--
--
{-# LANGUAGE TemplateHaskell #-}
module Main where


import Data.IntMap (IntMap)
import Data.Monoid (Endo(Endo), (<>), mempty)
import Control.Lens

import Common (Graph, readDGraph, printList)
import Neg

------------------------------------------------------------------------
-- Types and optics (top of file due to Template Haskell)
------------------------------------------------------------------------

data TarjanState = TarjanState
  { _nextIndex   :: Int
  , _stack       :: [Int]
  , _annotations :: IntMap VertexAnnotation
  }

data VertexAnnotation = VA
  { _vIndex, _vLowLink :: Int }

makeLenses ''TarjanState
makeLenses ''VertexAnnotation

------------------------------------------------------------------------
-- Rosalind.info format driver for SCC problem (comment out to load file)
------------------------------------------------------------------------

main :: IO ()
main = do
  g <- readDGraph
  printList (tarjan g)

------------------------------------------------------------------------
-- Tarjan's Strongly Connected Components Algorithms
------------------------------------------------------------------------

type Vertex = Int

-- The type of computations that stream [[Int]] in the context of a TarjanState
type C = Neg [[Int]] TarjanState TarjanState
--     = (TarjanState -> [[Int]]) -> TarjanState -> [[Int]]
--       continuation                state          result


tarjan :: Graph -> [[Vertex]]
tarjan g = runNeg (g^.ifolded.asIndex.to vertexStep)
                  (\_st -> [])
                  TarjanState { _nextIndex = 0, _stack = [], _annotations = mempty }
  where
  vertexStep :: Vertex -> C
  vertexStep v = branch $ \st ->
    whenE (hasn't (annotations.ix v) st)
      (strongConnect v)

  strongConnect :: Vertex -> C
  strongConnect v = start
                 <> g^.ix v.folded.to edgeStep
                 <> finish

    where
    -- first visit to this vertex, initialize its index and add to stack
    start :: C
    start = neg $ \st ->
               let i = st ^. nextIndex in
               st & nextIndex        +~ 1
                  & stack            %~ cons v
                  & annotations.at v ?~ VA i i

    -- consider a neighbor of v, recurse when unvisited, compare otherwise
    edgeStep :: Vertex -> C
    edgeStep w = branch $ \st ->
      case st^?annotations.ix w.vIndex of
        Nothing -> strongConnect w
                <> neg (annotations %~ vwMinLink w)
        Just wix ->
          whenE (w `elem` view stack st)
            (neg (annotations %~ minLink wix))

    -- emit a single SCC when v.lowlink=v.index
    finish :: C
    finish = branch $ \st ->
      case st ^?! annotations.ix v of
        VA vix vlow ->
          whenE (vix == vlow) $
            case break (==v) (st^.stack) of
              (xs,_v:s1) -> mapResult (cons (v:xs))
                         <> neg (stack .~ s1)

    -- v.lowlink = min v.lowlink w.lowlink
    vwMinLink :: Vertex -> IntMap VertexAnnotation -> IntMap VertexAnnotation
    vwMinLink w annot = minLink (annot ^?! ix w . vLowLink) annot

    -- v.lowlink = min v.lowlink i
    minLink :: Int -> IntMap VertexAnnotation -> IntMap VertexAnnotation
    minLink i = ix v . vLowLink %~ min i

------------------------------------------------------------------------
-- ENDO computation combinators
------------------------------------------------------------------------

whenE True  e = e
whenE False _ = mempty