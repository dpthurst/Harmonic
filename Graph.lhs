\documentclass[12pt]{amsart}
\input{preamble}
\input{defs}

\begin{document}

\begin{code}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
    FlexibleInstances, DeriveFunctor, ScopedTypeVariables
  #-}
module Graph where
import Prelude hiding ((!), reverse, elem, lookup)
import qualified Data.Map as Map
import Data.Map (Map, (!), fromList, lookup)
import qualified Data.Set as Set
import Data.Set (Set, union)
import qualified Data.List as List

import Mappable
\end{code}

Oriented edges know which direction they are facing.

\begin{code}
data Or a = Forward a | Backward a
            deriving (Show, Read, Eq, Ord, Functor)

unOr :: Or a -> a
unOr (Forward e) = e
unOr (Backward e) = e

instance Reversible (Or e) where
  reverse (Forward e) = Backward e
  reverse (Backward e) = Forward e
\end{code}

Graphs, in a fairly basic representation.

\begin{code}
data Graph v e = Graph { verticesSet :: Set v,
                         ends :: Map e (v, v) }
             deriving (Show, Read, Eq, Ord)

instance (Ord v, Ord e) => Arrow (Graph v e, e) v v where
    src (g, e) = fst ( (ends g)!e )
    tgt (g, e) = snd ( (ends g)!e )

instance (Ord v, Ord e) => Arrow (Graph v e, Or e) v v where
    src (g, Forward e)  = src (g, e)
    src (g, Backward e) = tgt (g, e)
    tgt (g, Forward e)  = tgt (g, e)
    tgt (g, Backward e) = src (g, e)

edgesSet :: (Ord v, Ord e) => Graph v e -> Set e
edgesSet g = Map.keysSet (ends g)

vertices :: (Ord v, Ord e) => Graph v e -> [v]
vertices g = Set.toList (verticesSet g)

edges :: (Ord v, Ord e) => Graph v e -> [e]
edges g = Map.keys (ends g)

orEdgesSet :: (Ord v, Ord e) => Graph v e -> Set (Or e)
orEdgesSet g = Set.map Forward (edgesSet g) `union` Set.map Backward (edgesSet g)

orEdges :: (Ord v, Ord e) => Graph v e -> [Or e]
orEdges g = map Forward (edges g) ++ map Backward (edges g)

neighborsSet :: (Ord v, Ord e) => Graph v e -> v -> Set (Or e)
neighborsSet g v = Set.filter (\e -> src (g,e) == v) (orEdgesSet g)

neighbors :: (Ord v, Ord e) => Graph v e -> v -> [Or e]
neighbors g v = Set.toList (neighborsSet g v)


prop_graph :: (Ord v, Ord e) => Graph v e -> Bool
prop_graph (Graph vs ends) =
    let ends' = Map.elems ends
    in (all (`Set.member` vs) (map fst ends' ++ map snd ends'))

makeGraph :: (Ord v, Ord e) => [(e, v, v)] -> Graph v e
makeGraph es =
    let vs = Set.fromList $ concat $ map (\(_,s,t) -> [s,t]) es
    in Graph { verticesSet = vs,
               ends = Map.fromList $ map (\(e,s,t) -> (e,(s,t))) es }

flowerGraph :: (Ord v, Ord e) => [(v, [Or e])] -> Graph v e
flowerGraph flowers =
    let oes = Map.fromList [(e,v) | (v,es) <- flowers, e <- es ]
        es = [ (e, v1, v2) | Forward e <- Map.keys oes,
                             let v1 = oes ! (Forward e)
                                 v2 = oes ! (Backward e) ]
    in makeGraph es
\end{code}

Maps on graphs.

\begin{code}
vmap :: (Ord v, Ord v', Ord e) => (v -> v') -> Graph v e -> Graph v' e
vmap f g = Graph { verticesSet = Set.map f (verticesSet g) ,
                   ends = Map.map (\(a,b) -> (f a, f b)) (ends g) }

emap :: (Ord v, Ord e, Ord e') => (e -> e') -> Graph v e -> Graph v e'
emap f g = Graph { verticesSet = verticesSet g,
                   ends = Map.mapKeys f (ends g) }

vemap :: (Ord v, Ord v', Ord e, Ord e') =>
         (v -> v') -> (e -> e') -> Graph v e -> Graph v' e'
vemap f1 f2 g = vmap f1 (emap f2 g)         
\end{code}
\end{document}
