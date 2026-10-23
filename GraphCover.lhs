\documentclass[12pt]{amsart}
\input{preamble}
\input{defs}

\begin{document}

\begin{code}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
    FlexibleInstances, DeriveFunctor, ScopedTypeVariables, TupleSections
  #-}
module GraphCover where
import Prelude hiding ((!), reverse, elem, lookup)
import qualified Data.Map as Map
import Data.Map (Map, (!))
import qualified Data.Set as Set
import Data.Set (Set, union)
import qualified Data.List as List

import Util
import Mappable
import Graph
import GraphMap
import IMatrix
import Permutation
\end{code}

\subsection*{Covers of graphs}
A cover of a graph is a graph where the vertices and edges are
labeled by pairs, and forgetting the second element of the pair gives
the downstairs graph (as in the function \texttt{pushOrEdge}).

\begin{code}
type GraphCover c v e = Graph (v,c) (e,c)

pushVert = fst
pushEdge = fst

pushOrEdge :: Or (e,c) -> Or e
pushOrEdge = fmap pushEdge

prop_graphCover :: forall c v e. (Ord c, Ord v, Ord e) =>
                   GraphCover c v e -> Bool
prop_graphCover g =
    let neighbors' :: (v,c) -> [Or e]
        neighbors' v = List.sort $ map pushOrEdge (neighbors g v)
        neighborMap :: Map v [[Or e]]
        neighborMap = Map.fromListWith (++)
                        [ (v, [ns]) | (v,c) <- vertices g,
                                      let ns = neighbors' (v,c) ]
    in all (\v -> allEqual (neighborMap Map.! v)) (Map.keys neighborMap)
\end{code}

One simple way to make a cover is to give a permutation for each    
edge. Covers are labelled by the value corresponding to their initial      
endpoint. Permutations that are not specified default to the identity.

\begin{code}
permCover :: forall c v e. (Ord c, Ord v, Ord e) =>
             Graph v e -> [c] -> [(e, Permutation c)] ->
             GraphCover c v e
permCover g dom emaps0 =
    let idDom = idP dom
        emaps = emaps0 ++ [(e,idDom) | e <- edges g List.\\ map fst emaps0]
    in makeGraph [((e,c1),(v1,c1),(v2,c2)) |
                 (e,p0) <- emaps,
                 let p = extendP dom p0,
                 let (v1,v2) = (ends g)!e,
                 c1 <- dom,
                 let c2 = imageP c1 p]

-- Shorthand for covers of various degrees
deg :: Int -> [Char]
deg n = take n (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'])
\end{code}

Given a vecotor of weights associated to the edges (usually either
lengths or elastic weights), we can get weights for the lifted graph.

\begin{code}
liftVec :: (Ord c, Ord v, Ord e) =>
           GraphCover c v e -> Vec e -> Vec (e,c)
liftVec g2 elast =
    makeVec (edges g2) [ ((e,c), elast `at` e) | (e,c) <- edges g2 ]
\end{code}

Lift an (oriented) edge to a covering space. The input is an edge
downstairs, and a lift of the source vertex of the edge.

\begin{code}
liftEdge :: forall c v e. (Ord c, Ord v, Ord e) =>
             GraphCover c v e -> Or e -> (v,c) -> Or (e,c)
liftEdge g2 e1 v2 =
    case List.find (\e2 -> e1 == pushOrEdge e2) (neighbors g2 v2) of
      Just e2 -> e2
      Nothing -> error "GraphCover.liftEdge: can't lift edge"
\end{code}

Lift a chain, a sequence of oriented edges from vertex to
vertex. (This is the path-lifting lemma!) The input is a chain
downstairs, and a lift of the initial vertex.

\begin{code}
liftChain :: forall c v e. (Ord c, Ord v, Ord e) =>
             GraphCover c v e -> Chain e -> (v,c) -> Chain (e,c)
liftChain g2 [] _ = []
liftChain g2 (e1:es) v2 =
    let e2 = liftEdge g2 e1 v2 in e2:liftChain g2 es (tgt (g2,e2))
\end{code}

Another version for chains possibly starting on an edge.

\begin{code}
liftChainEnd :: forall c v e. (Ord c, Ord v, Ord e) =>
                GraphCover c v e -> ChainEnd v e ->
                Either (v,c) (e,c) -> ChainEnd (v,c) (e,c)
liftChainEnd g2 (start,[],end) ve2 = (ve2,[],ve2)
liftChainEnd g2 (start,ch1,end) start2 =
    let v2 :: (v,c)
        v2 = case start2 of
               Left v2' -> v2'
               Right e2 -> case (head ch1) of
                             Forward _ -> src (g2,e2)
                             Backward _ -> tgt (g2,e2)
        ch2 = liftChain g2 ch1 v2
        end2 = case end of
                 Left _  -> Left (tgt (g2, last ch2))
                 Right _ -> Right (unOr (last ch2))
    in (start2,ch2,end2)
\end{code}

Now we can lift an entire graph. Really we are forming a pull-back:
\[
\begin{tikzpicture}
  \matrix[row sep=20pt, column sep=2em]{
    \node (G2c) {$\wt\Gamma_2$}; &
      \node (G1c) {$\wt\Gamma_1$};\\
    \node (G2) {$\Gamma_2$}; &
      \node (G1) {$\Gamma_1$};\\
  };
  \draw [->] (G2) -- node[cdlabel,above]{f} (G1);
  \draw [->] (G1c) -- (G1);
  \draw [dashed,->] (G2c) -- (G2);
  \draw [dashed,->] (G2c) -- node[cdlabel,above]{\wt{f}} (G1c);
\end{tikzpicture}
\]

We compute $\wt\Gamma_2$ and $\wt f$ in sequence:
\begin{itemize}
\item The \emph{vertices} of $\wt\Gamma_2$ are just pairs $(v_2,c)$,
  where $v_2$ is a vertex of $\Gamma_2$ and $c$ is the covering set.
\item For a vertex $(v_2,c)$ of $\wt\Gamma_2$, if $f(v_2)$ is a vertex
  $v_1$, then $\wt f(v_2,c) = (v_1,c)$; likewise, if $f(v_2)$ is an edge
  $e_1$, then $\wt f(v_2,c) = (e_1,c)$. (This amounts to a choice of how
  to index the vertices of $\wt\Gamma_2$ lying above $v_2$.)
\item Next, for an edge $(e_2,c)$ of $\wt\Gamma_2$, we choose the
  indexing so that $\mathtt{src}(e_2,c) = (\mathtt{src}(e_2),
  c)$. This then forces what $\wt f(e_2,c)$ does (using the
  \texttt{liftChainEnd} function above), and in particular where the
  final endpoint lifts to.
\end{itemize}

\begin{code}
type GraphMapCover c v1 e1 v2 e2 = GraphMap (v1,c) (e1,c) (v2,c) (e2,c)

liftMap :: forall c v1 e1 v2 e2. (Ord c, Ord v1, Ord e1, Ord v2, Ord e2) =>
           GraphCover c v1 e1 ->
           (Graph v2 e2, GraphMap v2 e2 v1 e1) ->
           (GraphCover c v2 e2, GraphMapCover c v2 e2 v1 e1)
liftMap g1c (g2, f) =
    let v0 :: v1; v0 = fst $ head $ vertices g1c
        cs :: [c]; cs = map snd $ filter (\v -> fst v == v0) $ vertices g1c
        c0 = head cs
        verts' :: Set (v2,c)
        verts' = Set.fromList [ (v2,c) | v2 <- vertices g2, c <- cs ]
        vm' :: VertexMap (v2,c) (e2,c) (v1,c) (e1,c)
        vm' = Map.fromList [ ((v2,c), mapEV (,c) (,c) ev1) |
                             (v2,ev1) <- Map.toList (vm f), c <- cs ]
        em' :: EdgeMap (v2,c) (e2,c) (v1,c) (e1,c)
        ends' :: Map (e2,c) ((v2,c),(v2,c))
        (em',ends') = (\(x,y) -> (Map.fromList x, Map.fromList y)) $ unzip $
              do e2 <- edges g2
                 let ch1@(start1,_,_) = mapChainEnd g2 f (Forward e2)
                 c <- cs; let e2c = (e2,c)
                 let start1c = mapEV (,c) (,c) start1
                 let (_,ch1c,end1c) = liftChainEnd g1c ch1 start1c
                 let c' = either snd snd end1c
                 return  ((e2c, ch1c),
                          (e2c, ((src (g2,e2),c), (tgt (g2,e2),c'))))
    in (Graph verts' ends', GM vm' em')
\end{code}

We will want to iterate covers. For this, we store a list of cover indices.

\begin{code}
type GraphIterCover c v e = GraphCover [c] v e

trivCover :: forall c v e. (Ord c, Ord v, Ord e) =>
             Graph v e -> GraphIterCover c v e
trivCover g = vemap ( , [] ) ( , [] ) g

simpleCover :: forall c v e. (Ord c, Ord v, Ord e) =>
             GraphIterCover c v e -> GraphCover c (v,[c]) (e,[c])
simpleCover g = let f (a,c1:cs) = ((a,cs), c1) in vemap f f g

iterCover :: forall c v e. (Ord c, Ord v, Ord e) =>
             GraphCover c (v,[c]) (e,[c]) -> GraphIterCover c v e
iterCover g = let f ((a,cs),c1) = (a,c1:cs) in vemap f f g
\end{code}

Now we want to iterate covers repeatedly.
\begin{code}
type VirtualEndo c v e = (GraphCover c v e, GraphMap (v,c) (e,c) v e)

iterateEndo :: forall c v e. (Ord c, Ord v, Ord e) =>
               (Graph v e,VirtualEndo c v e) -> [VirtualEndo [c] v e]
iterateEndo (g0,(g1, f1)) =
    let fiddle (a, c1:cs) = ((a,cs),c1)
        step :: VirtualEndo [c] v e -> VirtualEndo [c] v e
        step (gn, fn) =
            let (g',f') = liftMap g1 (gn, fn)
                gnp = iterCover g'
                remix :: GraphMap (v,[c]) (e,[c]) ((v,[c]),c) ((e,[c]),c)
                remix = idVEMap fiddle fiddle gnp g'
                fnp :: GraphMap (v,[c]) (e,[c]) v e
                fnp = composeMap gnp g' (composeMap g' g1 f1 f') remix
            in (gnp, fnp)
        g0' = trivCover g0
    in iterate step (g0', idVEMap fst fst g0' g0)
\end{code}

\end{document}
