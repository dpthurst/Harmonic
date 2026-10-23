\documentclass[12pt]{amsart}
\input{preamble}
\input{defs}

\begin{document}

Here we look at some examples where the Julia set is the whole
sphere. The goal is to investigate convergence of extremal lengths        
under iteration, and whether they converge to a multiple of extremal      
length on the actual complex surface.

\begin{code}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
    FlexibleInstances, DeriveFunctor, ScopedTypeVariables,
    ParallelListComp
  #-}
module Julia where
import Mappable
import Graph
import GraphMap
import GraphCover
import GraphIO
import IMatrix
import Harmonic
import Filling
import OptFilling
import Util
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.List as List
import qualified Data.Char as Char
import Circle
import Torus
import Text.Printf -- For utility use in browser
\end{code}

\section{Example 1: Quadratic, $(4,4,4,2)$ orbifold}

We consider the function
\[
f(z) = \frac{2z^2-4}{z^2}.
\]
This has critical portrait
\[
\mathcenter{\begin{tikzpicture}[node distance=1.4cm]
  \node (0) {$0$};
  \node[Mahogany] (infty) [right of=0] {$\infty$};
  \node[Mahogany] (2) [right of=infty] {$2$};
  \node[Mahogany] (1) [right of=2] {$1$};
  \node[Mahogany] (neg2) [right of=1] {$-2$};
  \draw[Mahogany,|->] (0) to node[above,cdlabel]{(2)} (infty);
  \draw[Mahogany,|->] (infty) to node[above,cdlabel]{(2)} (2);
  \draw[Mahogany,|->] (2) to (1);
  \draw[Mahogany,|->,bend left] (1) to (neg2.160);
  \draw[Mahogany,|->,bend left] (neg2.200) to (1);
  \node (sq2) [above left of=0] {$\sqrt{2}$};
  \node (negsq2) [below left of=0] {$-\sqrt{2}$};
  \draw[|->] (sq2) to (0);
  \draw[|->] (negsq2) to (0);
  \node (neg1) [right of=neg2] {$-1$};
  \draw[|->] (neg1) to (neg2);
  \node (p1) [above right of=neg1] {$\frac{2}{\sqrt{3}}$};
  \node (p2) [below right of=neg1] {$-\frac{2}{\sqrt{3}}$};
  \draw[|->] (p1) to (neg1);
  \draw[|->] (p2) to (neg1);
\end{tikzpicture}}.
\]
(The post-critical set is in red, and a few inverse images have been added.)

Since the function is post-critically finite with no critical periodic
cycles, the Julia set is the whole sphere. The orbifold is hyperbolic,
with cone points $(4,4,4,2)$.

TODO: Draw $\Gamma$, $f^{-1}(\Gamma)$. See DPT notes, 2015-02-12 \#6.

In code, this is the graph.
\begin{code}
(f1_0,(f1_1,f1_map)) = readEndo "apqrs bSRQP" "q01" "r0P s0Q p0Q p1q s1r r1s"
f1_roseMap = readMap f1_0 rose3 "pP rq sqr"
f1 = (f1_0, f1_1, f1_map, f1_roseMap)
\end{code}

The actual rational map has post-critical points at
$\{\infty,-2,1,2\}$. By computing the Schwarz--Christoffel integral
\[
\int \frac{dz}{\sqrt{(z^2-4)(z-1)}}
\]
we can see that the sphere with these four marked points is equivalent
to a rectangle of aspect ratio $1.279$ doubled along its boundary. The
ratio
\[
\frac{\text{extremal length of horizontal loop}}
     {\text{extremal length of vertical loop}}
\]
is therefore $(1.279)^2 = 1.637$.

On the other hand, we can iterate this dynamical system and compute
this ratio of extremal lengths at each stage. The results appear to
converge to a number in the same ballpark:
\[
\begin{tabular}{clll}
  \toprule
  Iterate&$\EL_v$&$\EL_h$&Ratio\\\midrule
  0 & 0.5 & 1 & 2\\
  1 & 0.4 & 0.636 & 1.591 \\
  2 & 0.378 & 0.646 & 1.706 \\
  3 & 0.278 & 0.448 & 1.611 \\
  4 & 0.252 & 0.419 & 1.662 \\
  5 & 0.190 & 0.306 & 1.610 \\
  6 & 0.165 & 0.272 & 1.647 \\
  7 & 0.128 & 0.205 & 1.609 \\ \bottomrule
\end{tabular}
\]
The starting elastic weights on the four edges were $(2,1,2,1)$,
chosen somewhat arbitrarily. Different values give better or worse
convergence rates, but all appear consistent with the conjectured
convergence. Note that the extremal lengths themselves appear to
converge to~$0$.

\section{Example 2: Quadratic, $(4,4,2,2)$ orbifold}

Consider the function
\[
f(z) = \frac{1 + \frac{z^2}{1-i}}{1-z^2}.
\]
This has critical portrait:
\[
\begin{tikzpicture}[node distance=1.6cm]
  \node (0) {$0$};
  \node (1) [Mahogany,right of=0] {$1$};
  \node (infty) [Mahogany,right of=1] {$\infty$};
  \node (p1) [Mahogany, right of=infty] {$\frac{-1-i}{2}$};
  \node (p2) [Mahogany, right of=p1] {$\frac{1+i}{2}$};
  \draw[Mahogany,|->] (0) to node[above,cdlabel]{(2)} (1);
  \draw[Mahogany,|->] (1) to (infty);
  \draw[Mahogany,|->] (infty) to node[above,cdlabel]{(2)} (p1);
  \draw[Mahogany,|->] (p1) to node[above,cdlabel]{(2)} (p2);
  \draw[Mahogany,|->,loop below] (p2) to (p2);
%  \node(p3) [below of=infty] {$-1$};
%  \draw[|->] (p3) to (infty);
\end{tikzpicture}
\]
The Julia set is again the whole sphere, with hyperbolic orbifold $(4,4,2,2)$.
(Up to conjugation, this is the unique quadratic rational map with
this orbifold.)

For the graph, see DPT notes, 2015-02-14 \#1. Here it is in code.
\begin{code}
(f2_0,(f2_1,f2_map)) =
    readEndo "apqr bPQst cTSR" "q01 s01" "q0p s0q s1Q q1R r1S p1T"
f2_roseMap = readMap f2_0 rose3 "pp tq rR"
f2 = (f2_0, f2_1, f2_map, f2_roseMap)
\end{code}

The result here is not rectangular, as the post-critical points are
not all on the real axis. We get the following values for expected
side lengths of the triangle shapes under iteration. These numbers
are square roots of extremal lengths. The starting elastic lengths are
$1$ on each edge.
\[
\begin{tabular}{clll}
  \toprule
  Iterate & $\alpha$ & $\beta$ & $\gamma$ \\ \midrule
  0 & 1.0 & 1.0    & 1.1547 \\
  1 & 1.0 & 1.25   & 1.1990 \\
  2 & 1.0 & 1.1959 & 1.3308 \\
  3 & 1.0 & 1.1359 & 1.1878 \\
  4 & 1.0 & 1.2452 & 1.2843 \\
  5 & 1.0 & 1.1725 & 1.2646 \\
  6 & 1.0 & 1.1916 & 1.2349 \\
  7 & 1.0 & 1.2116 & 1.2759 \\
  \midrule
  Expected & 1.0 & 1.19586 & 1.25727 \\
  \bottomrule
\end{tabular}
\]
The expected answer is computed as:
\begin{align*}
  \beta &=
   \left\lvert\frac{\int_0^{-2i}\frac{dz}{\sqrt{z(z-1)(z+2i)}}}
                   {\int_0^{1} \frac{dz}{\sqrt{z(z-1)(z+2i)}}}\right\lvert &
  \gamma &=
   \left\lvert\frac{\int_1^{-2i}\frac{dz}{\sqrt{z(z-1)(z+2i)}}}
                   {\int_0^{1} \frac{dz}{\sqrt{z(z-1)(z+2i)}}}\right\lvert &
\end{align*}
That is, find the piecewise Euclidean structure on $\widehat\CC$ with
cone points of angle~$\pi$ at $\{0,1,2i,\infty\}$ (which are equivalent to
$\{(1+i)/2,1,-(1+i)/2,\infty\}$), and find the shape of the triangle
with vertices at $\{0,1,2i\}$.

\section{Computing extremal lengths}
These are just glue functions to call the functions in \texttt{Torus.lhs}.
The output of \texttt{endoRatios} gives the shape of the fundamental
triangle (after taking square roots).

\begin{code}
endo (f_0, f_1, f_map,_) = (f_1, f_map)
endos f@(f_0, f_1, f_map,_) = iterateEndo (f_0,(endo f))

endoEL (g0, g1, f, rmap) es nm =
    let elast0 = edgeVec g0 es
        elast1 = liftVec g1 elast0
    in elTorus (g1, elast1) (composeMap g1 g0 rmap f) nm

endoELs f@(g0, g1, f1, rmap) es nms =
   [[endoEL (g0, gn, fn, rmap) es nm | nm <- nms] | (gn, fn) <- endos f]

endoRatios f es nms =
   map (\(v:vs) -> 1:map (/v) vs) (endoELs f es nms)

triangShapes f es = map (map sqrt) (endoRatios f es stdnms)

stdnms = [(1,0),(0,1),(1,1)] :: [(Int,Int)]
\end{code}

\end{document}
