\documentclass[12pt]{amsart}
\input{preamble}
\input{defs}

\begin{document}

\begin{code}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
    FlexibleInstances, DeriveFunctor, ScopedTypeVariables
  #-}
module Filling where

import Mappable
import Graph
import GraphMap
import GraphCover
import IMatrix
import Harmonic
import Util
import Data.List (dropWhile, tails, iterate)
\end{code}

This module finds the $\lambda$-filling representative of a homotopy
class of graph maps, or a good approximation thereof. It is currently
limited: the actual $\lambda$-filling representative is only filling
on a subgraph, and we do not explicitly identify this subgraph.

We think about finding a $\lambda$-filling representative as the
problem of finding a set of lengths that maximizes the ratio of
harmonic energies.

The following iteration
appears to converge.
\begin{enumerate}
\setcounter{enumi}{-1}
\item Pick a set of widths $v_0 \in \Widths^+(\Gamma_2)$, i.e., a width for
  each edge of $\Gamma_2$ satisfying the triangle inequality.
\item\label{item:step-K} Take the metric graph $K_0$ to be $\Gamma_2$ with edge $e$
  assigned length $\alpha(e) v_0(e)$. Note that the evident map $f_0 \co
  \Gamma_2\to K_0$ is harmonic.
\item Find a harmonic representative~$g_0$ of the composite map $\Gamma_1
  \overset{\phi}{\to} \Gamma_2 \overset{f}{\to} K_0$. Our first
  approximation to~$\psi$ is $\psi_0 = (f_0)^{-1} \circ g_0$.
\item Compute the tension of~$g_0$ (i.e., $\abs{g_0'(x)}$) in each
  edge of~$\Gamma_1$, getting $w_0 \in \Widths^+(\Gamma_1)$.
\item Push forward $w_0$ to a function~$v_1 \in \Widths^+(\Gamma_2)$ by
  setting, for $e \in \Edges(\Gamma_2)$ and $y \in e$,
  \[
  v_1(y) = \sum_{x \in g^{-1}(f(y))} w_0(x).
  \]
  This is independent of the choice of~$y$ since $g$ is harmonic. Then
  return to Step~\eqref{item:step-K}, using $v_1$ instead of~$v_0$.
\end{enumerate}
Schematically, we are iterating around the following cycle:
\[
\begin{tikzpicture}[baseline]
  \matrix[row sep=30pt, column sep=3em]{
    \node (L2) {$\Lengths(\Gamma_2)$};
      & \node (W2) {$\Widths^+(\Gamma_2)$};\\
    \node (L1) {$\Lengths(\Gamma_1)$};
      & \node (W1) {$\Widths^+(\Gamma_1)$};\\
  };
  \draw [->] (L2) -- node[left]{\small (2)} (L1);
  \draw [->] (L1) -- node[above]{\small (3)} (W1);
  \draw [->] (W1) -- node[right]{\small (4)} (W2);
  \draw [->] (W2) -- node[above]{\small (1)} (L2);
\end{tikzpicture}.
\]
At each step, we can compute the embedding energy:
\[
\Emb(\psi_i) = \max_{e \in \Edges(\Gamma_1)} \frac{v_{i+1}(e)}{v_i(e)}.
\]
TODO: Actually do that computation; maximum of ratios of widths.

\begin{code}
type FillingMap v1 e1 v2 e2 = (PosMap v1 e1 v2 e2, Vec e2)

stepFilling :: forall v1 e1 v2 e2. (Ord v1, Ord e1, Ord v2, Ord e2) =>
               ElasticGraph v1 e1 -> ElasticGraph v2 e2 ->
               FillingMap v1 e1 v2 e2 ->
               (FillingMap v1 e1 v2 e2, Double)
stepFilling elast1@(g1,e1) elast2@(g2,e2) ((f,_), t2) =
    let l2 = elasticMatrix e2 # t2
        fp' = findHarmonic elast1 (g2,l2) f
        t1 = tensions elast1 (g2,l2) fp'
        t2' = (transpose (lengthMap g1 g2 (fst fp'))) # t1
        n2' = norm t2'; t2'' = scale (1/n2') t2'
    in ((fp', t2''),n2')

iterFilling :: forall v1 e1 v2 e2. (Ord v1, Ord e1, Ord v2, Ord e2) =>
               ElasticGraph v1 e1 -> ElasticGraph v2 e2 ->
               PosMap v1 e1 v2 e2 -> [(FillingMap v1 e1 v2 e2, Double)]
iterFilling elast1 elast2@(g2,_) fp0 =
    let wid0 = makeOneVec (edges g2) in
    iterate (\(fp,n) -> stepFilling elast1 elast2 fp) ((fp0,wid0),1)
\end{code}

Iterate until the $\lambda$-filling factor doesn't change very
much. Start with an arbitrary starting point.

\begin{code}
findFilling :: forall v1 e1 v2 e2. (Ord v1, Ord e1, Ord v2, Ord e2) =>
               ElasticGraph v1 e1 -> ElasticGraph v2 e2 ->
               GraphMap v1 e1 v2 e2 ->
               (FillingMap v1 e1 v2 e2, Double)
findFilling elast1 elast2 f0 =
    let continue ((f1,n1):(f2,n2):_) = abs ((n1-n2)/n1) > eps4
        continue _ = False
    in head $ head $ tail $ dropWhile continue $ tail $ tails $ iterFilling elast1 elast2 (f0,error "Position not used")
\end{code}

For a virtual endomorphism, lift the elastics.

\begin{code}
endoFilling :: forall v e c. (Ord v, Ord e, Ord c) =>
               ElasticGraph v e -> VirtualEndo c v e ->
               (FillingMap (v,c) (e,c) v e, Double)
endoFilling (g0,elast0) (g1,f1) =
    findFilling (g1, liftVec g1 elast0) (g0, elast0) f1

endoFillings :: forall v e c. (Ord v, Ord e, Ord c) =>
                ElasticGraph v e -> VirtualEndo c v e ->
                [(Graph (v,[c]) (e,[c]),
                  FillingMap (v,[c]) (e,[c]) v e, Double)]
endoFillings elast0@(g0,_) endo =
    let endos = iterateEndo (g0,endo)
        fillings = map (endoFilling elast0) endos
    in zipWith (\(a,_) (b,c) -> (a,b,c)) endos fillings

getSFs :: [(a,b,c)] -> [c]
getSFs = map (\(a,b,c) -> c)

endoSFs (graph,endo) elast = getSFs $
                             endoFillings (elasticGraph graph elast) endo
\end{code}

\end{document}
