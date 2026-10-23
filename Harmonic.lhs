\documentclass[12pt]{amsart}
\input{preamble}
\input{defs}

\begin{document}

\begin{code}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
    FlexibleInstances, DeriveFunctor, ScopedTypeVariables
  #-}
module Harmonic where
import Prelude hiding ((!), reverse, elem, lookup)
import qualified Data.Map as Map
import Data.Map (Map, (!), fromList, lookup)
import qualified Data.Set as Set
import Data.Set (Set, union)
import qualified Data.List as List
import Data.Maybe(mapMaybe, catMaybes)

import Mappable
import Graph
import GraphMap
import IMatrix
import Util

type Lengths e = Vec e
type LengthGraph v e = (Graph v e, Lengths e)
type Elastics e = Vec e
type ElasticGraph v e = (Graph v e, Elastics e)
type PosMap v1 e1 v2 e2 = (GraphMap v1 e1 v2 e2, Vec v1)
\end{code}

Recall that a graph map $f: \Gamma_1 \to \Gamma_2$ records maps
\begin{align*}
  \Verts(\Gamma_1) &\longrightarrow \Verts(\Gamma_2) \sqcup \Edges(\Gamma_2)\\
  \OrEdges(\Gamma_1) &\longrightarrow [\OrEdges(\Gamma_2)]
\end{align*}

On a given graph~$\Gamma$, we can consider the space of \emph{lengths}
$\Lengths(\Gamma)$, or the space of \emph{widths} $\Widths(\Gamma)
\simeq \Lengths(\Gamma)^*$. Inside the space of widths there is a
subspace $\Widths^+(\Gamma)$ which satisfy the triangle inequalities.

Given a set of lengths on $\Gamma_2$ and a set of elastic weights on
$\Gamma_1$, we want to find the harmonic representative. This involves
linear algebra to find the actual positions of the vertices that map
to edges. The basic data is
\begin{itemize}
\item The lengths on the target graph, $\ell_2 \in \Lengths(\Gamma_2)$.
\item The elastic weights on the domain graph, which we can
  think about as a map $Q_1: \Widths(\Gamma_1) \to
  \Lengths(\Gamma_1)$. The actual elastic energy if the edges are
  stretched as $\ell_1 \in \Lengths(\Gamma_1)$ is $\langle \ell_1,
  (Q_1)^{-1}\ell_1\rangle$.
\item The actual positions of the vertices of $\Gamma_1$ that map to
  edges of $\Gamma_2$. This is a map $p: V_e \to \RR$, where $V_e
  \subset \Verts(\Gamma_1)$, and is the variable that we optimize for
  in the linear map. (The value is the position along the edge of
  $\Gamma_2$, ranging from $0$ to the length of the edge.) Let
  $\Pos(f)$ be this linear space.
\end{itemize}
The combinatorics of the map give us two linear maps; in both cases
the transpose is interesting.
\begin{itemize}
\item A map $L: \Lengths(\Gamma_2) \to \Lengths(\Gamma_1)$, giving the
  lengths of edges in $\Gamma_1$ as a function of the lengths in
  $\Gamma_2$, assuming that all vertices that map to edges map to the
  initial endpoint of that edge (i.e., corresponding to $0 \in
  \Pos(f)$).
\item A map $V: \Pos(f) \to \Lengths(\Gamma_1)$ that gives the length
  correction coming from more general maps, so that the total length
  of edges for $\Gamma_1$ is $\ell_1 = L\ell_2 + Vp$.
\item The transpose $V^t: \Widths(\Gamma_1) \to \Pos(f)^*$ controls
  the train-track condition: $w_1 \in \Widths(\Gamma_1)$ satisfies one
  of the train-track conditions if $V^t w_1 = 0$. (The other condition
  is the triangle inequality.)
\item The transpose $L^t: \Widths(\Gamma_1) \to \Widths(\Gamma_2)$
  gives the total width of an edge in $\Gamma_2$ given the widths of
  the edges in $\Gamma_1$. (NB: Plays well with train-track condition.)
\end{itemize}

These maps fit into a diagram:
\[
\begin{tikzpicture}
  \matrix[row sep=30pt, column sep=3em]{
    \node (L2) {$\mathllap{\ell_2\in{}}\Lengths(\Gamma_2)$}; 
      & \node (W2) {$\Widths(\Gamma_2)$}; \\
    \node (L1) {$\Lengths(\Gamma_1)$};
      & \node (W1) {$\Widths(\Gamma_1)\mathrlap{{} \simeq \Lengths(\Gamma_1)^*}$};\\
    \node (P) {$\mathllap{p \in {}}\Pos(f)$}; 
      & \node (Pt) {$\Pos(f)^*$}; \\
  };
  \draw [->] (L2) -- node[cdlabel,left]{L} (L1);
  \draw [->] (W1) -- node[cdlabel,above]{Q_1} (L1);
  \draw [->] (P) -- node[cdlabel,left]{V} (L1);
  \draw [->] (W1) -- node[cdlabel,right]{V^*} (Pt);
  \draw [->] (W1) -- node[cdlabel,right]{L^*} (W2);
\end{tikzpicture}
\]

We start by defining the $L$ and $V$ maps.
\begin{code}
lengthMap :: (Ord v1, Ord e1, Ord v2, Ord e2) =>
             Graph v1 e1 -> Graph v2 e2 -> GraphMap v1 e1 v2 e2 ->
             IMatrix e2 e1
lengthMap s t f =
    let es1 = edges s
        es2 = edges t
        entries1 = [((unOr e2, e1), 1) | e1 <- es1, e2 <- (em f)!e1 ]
        entries2 = [((e2, unOr e1), -0.5) | v <- vertices s, isE f v,
                                            let Right e2 = (vm f)!v,
                                            e1 <- neighbors s v,
                                            not (null (f # e1)) ]
    in makeIMatrix es2 es1 (entries1 ++ entries2)

vertexMap :: forall v1 e1 v2 e2. (Ord v1, Ord e1, Ord v2, Ord e2) =>
             Graph v1 e1 -> Graph v2 e2 -> GraphMap v1 e1 v2 e2 ->
             IMatrix v1 e1
vertexMap s t f =
    let vs :: [v1]
        vs = [v | v <- vertices s, Right _ <- [(vm f)!v] ]
        es :: [e1]
        es = edges s
        coeff :: v1 -> Or e1 -> Double
        coeff v0 e0 =
            case f # e0 of
              (Forward  _:_) -> -1
              (Backward _:_) -> 1
              [] -> case e0 of
                      Forward  _ -> -1
                      Backward _ -> 1
        entries = [((v, unOr e), coeff v e) | v <- vs, e <- neighbors s v]
    in makeIMatrix vs es entries
\end{code}

Find the actual imputed lengths and tensions given a set of
positions. Currently this is not used as much as it should be.

\begin{code}
elasticMatrix, elasticInv :: (Ord e) => Elastics e -> IMatrix e e
elasticMatrix elast = diagonal elast
elasticInv    elast = diagonal (mapIMatrix (\x -> 1/x) elast)

lengths, tensions, energies :: (Ord v1, Ord e1, Ord v2, Ord e2) =>
     ElasticGraph v1 e1 -> LengthGraph v2 e2 ->
     PosMap v1 e1 v2 e2 -> Vec e1
lengths (s,elast) (t,len) (f, ps) = lengthMap s t f # len + vertexMap s t f # ps
tensions s@(_,elast) t f = elasticInv elast # lengths s t f
energies s@(_,elast) t f =
    zipIMatrix (\alpha l -> l*l/alpha) elast (lengths s t f)

energy :: (Ord v1, Ord e1, Ord v2, Ord e2) =>
          ElasticGraph v1 e1 -> LengthGraph v2 e2 ->
          PosMap v1 e1 v2 e2 -> Double
energy s t f = let ls = lengths s t f in
               ((transpose ls) # elasticInv (snd s) # ls) # (One,One)
\end{code}

Given an elastic graph structure on~$\Gamma_1$ (determining $Q_1$
above), the combinatorics of the graph map (determining $L$ and~$V$),
and the lengths on $\Gamma_2$ (determining $\ell_2$), we now do the    
linear algebra to find the optimum positions given the
combinatorics of the map. We want to minimize
\begin{align*}
E(p) &= \bigl\langle L\ell_2 + Vp, Q_1^{-1}(L\ell_2 + V p)\bigr\rangle,\\
\intertext{with derivative}
dE &= \bigl\langle V\,dp, Q_1^{-1}(L\ell_2 + V p)\bigr\rangle\\
   &= \bigl\langle dp, V^* Q_1^{-1}(L\ell_2 + V p)\bigr\rangle
\end{align*}
and so we must have
\[
V^*Q_1^{-1}Vp = -V^*Q_1^{-1}L\ell_2.
\]
These constraints might well be degenerate. We look for the minimal      
norm solution, which we set up so that it will have vertices as close     
to the centers of edges as possible.

\begin{note}
  The solution may well have the position of vertices off the ends of
  the corresponding edges. This can happen even if there is a solution
  with all points in the interior, as the linear equations may be
  degenerate. We try to minimize the chances of this happening by
  centering the linear equations around the centers of the edges
  (since the underlying solver picks the solution of minimal
  norm). Probably we end up in a position where enough vertices are
  (needlessly) frozen. Really we should be solving a convex quadratic  
  programming problem (minimizing a positive-definite quadratic  
  function on a convex set). There are known efficient algorithms for  
  that, but it is not currently implemented.
\end{note}

\begin{code}
findPos :: (Ord v1, Ord e1, Ord v2, Ord e2) =>
           ElasticGraph v1 e1 -> LengthGraph v2 e2 ->
           GraphMap v1 e1 v2 e2 -> Vec v1
findPos (g1,e1) (g2,l2) f =
    let lm = lengthMap g1 g2 f; vm = vertexMap g1 g2 f; em = elasticInv e1
        l1_0 = lm # l2
        v1 = negate $ solve (transpose vm # em # vm) (transpose vm # em # l1_0)
    in v1
\end{code}

Now we check whether the solution is valid, in two steps. First we
check if any vertices got pulled off the end of the corresponding
edge, and update the combinatorial map if so.

\begin{code}
pushVertices :: forall v1 e1 v2 e2. (Ord v1, Ord e1, Ord v2, Ord e2) =>
                ElasticGraph v1 e1 -> LengthGraph v2 e2 ->
                PosMap v1 e1 v2 e2 -> Maybe (GraphMap v1 e1 v2 e2)
pushVertices (g1, e1) (g2, l2) (f, ps) =
    let evs = filter (isE f) (vertices g1)
        checkV :: v1 -> Maybe (v1, Or e2)
        checkV v =
            let p = ps `at` v; Right e = (vm f)!v in
            if p < (-0.5-eps4)*(l2 `at` e) then Just (v, Backward e)
            else if p > (0.5+eps4)*(l2 `at` e) then Just (v, Forward e)
            else Nothing
        updates = mapMaybe checkV evs
    in case updates of
         [] -> Nothing
         _  -> Just (foldl (pushVertex g1 g2) f updates)
\end{code}

Next, the triangle inequality at vertices might be violated.

\begin{code}
pullVertices :: forall v1 e1 v2 e2. (Ord v1, Ord e1, Ord v2, Ord e2) =>
                ElasticGraph v1 e1 -> LengthGraph v2 e2 ->
                PosMap v1 e1 v2 e2 -> Maybe (GraphMap v1 e1 v2 e2)
pullVertices s@(g1,e1) t@(g2,l2) fp@(f, ps) =
    let vvs = filter (isV f) (vertices g1)
        t1 = tensions s t fp
        pullDir :: Or e1 -> Maybe (Or e2)
        pullDir e1 = case f # e1 of { (e2:_) -> Just e2; [] -> Nothing }
        checkV :: v1 -> Maybe (v1, Or e2)
        checkV v =
            let ns = neighbors g1 v
                ts = catMaybes $ map
                      (\e1 -> do { e2 <- pullDir e1;
                                  return (e2, t1 `at` (unOr e1)) } ) ns
            in do { e <- checkTriangle ts; return (v,e) }
        updates :: [(v1, Or e2)]
        updates = catMaybes $ map checkV vvs
    in case updates of
         [] -> Nothing
         _  -> Just (foldl (pullVertex g1 g2) f updates)
\end{code}

Now to put it all together and find the harmonic optimum. Guard
against looping.

\begin{code}
pushPull :: forall v1 e1 v2 e2. (Ord v1, Ord e1, Ord v2, Ord e2) =>
            ElasticGraph v1 e1 -> LengthGraph v2 e2 ->
            PosMap v1 e1 v2 e2 -> Maybe (GraphMap v1 e1 v2 e2)
pushPull eg1 lg2 fp =
    case pushVertices eg1 lg2 fp of {
       Just f2 -> Just f2;
       Nothing -> pullVertices eg1 lg2 fp
     }

stepHarmonic :: forall v1 e1 v2 e2. (Ord v1, Ord e1, Ord v2, Ord e2) =>
                ElasticGraph v1 e1 -> LengthGraph v2 e2 ->
                PosMap v1 e1 v2 e2 -> Maybe (PosMap v1 e1 v2 e2)
stepHarmonic eg lg fp =
    do f' <- pushPull eg lg fp
       let ps' = findPos eg lg f'
       return (f', ps')

findHarmonic :: forall v1 e1 v2 e2. (Ord v1, Ord e1, Ord v2, Ord e2) =>
                ElasticGraph v1 e1 -> LengthGraph v2 e2 ->
                GraphMap v1 e1 v2 e2 -> PosMap v1 e1 v2 e2
findHarmonic elast len f0 =
    let p0 = findPos elast len f0 in
    last $ take 200 $ iterateMaybe (stepHarmonic elast len) (Just (f0, p0))

findEnergy :: forall v1 e1 v2 e2. (Ord v1, Ord e1, Ord v2, Ord e2) =>
              ElasticGraph v1 e1 -> LengthGraph v2 e2 ->
              GraphMap v1 e1 v2 e2 -> Double
findEnergy elast len f0 = energy elast len $ findHarmonic elast len f0
\end{code}

Utility, useful for making lengths or elastics associated to a graph.

TODO: move this elsewhere.

\begin{code}
edgeVec :: (Ord v, Ord e) => Graph v e -> [Double] -> Vec e
edgeVec g vals = makeVec (edges g) (zip (edges g) vals)

lengthGraph :: (Ord v, Ord e) => Graph v e -> [Double] -> LengthGraph v e
lengthGraph g vals = (g, edgeVec g vals)

elasticGraph :: (Ord v, Ord e) => Graph v e -> [Double] -> ElasticGraph v e
elasticGraph = lengthGraph
\end{code}
\end{document}
