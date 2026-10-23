\documentclass[12pt]{amsart}
\input{preamble}
\input{defs}

\begin{document}

Here, we optimize the filling energy for an endomorphism.

\begin{code}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
    FlexibleInstances, DeriveFunctor, ScopedTypeVariables
  #-}
module OptFilling where

import Mappable
import Graph
import GraphMap
import GraphCover
import IMatrix
import Harmonic
import Filling
import Util
\end{code}

First, compute the gradients of a filling map with respect to elastic
lengths on the source and target. The logarithmic derivatives are
proportional to the energy given by each edge; this translates to the
following formulas.

\begin{code}
tgtDeriv :: forall v1 e1 v2 e2. (Ord v1, Ord e1, Ord v2, Ord e2) =>
    ElasticGraph v1 e1 -> ElasticGraph v2 e2 ->
    FillingMap v1 e1 v2 e2 -> e2 -> Double
tgtDeriv eg1 (g2,elast2) (m,w2) e0 =
    let w e = w2 `at` e
        alpha e = elast2 `at` e
        energy = sum [w e * w e * alpha e | e <- edges g2]
    in w e0*w e0/energy

tgtGradient :: forall v1 e1 v2 e2. (Ord v1, Ord e1, Ord v2, Ord e2) =>
    ElasticGraph v1 e1 -> ElasticGraph v2 e2 ->
    FillingMap v1 e1 v2 e2 -> Vec e2
tgtGradient eg1 (g2,elast2) (m,w2) =
    let w e = w2 `at` e
        alpha e = elast2 `at` e
        energy = sum [w e * w e * alpha e | e <- edges g2]
    in scale (1/energy) $ zipIMatrix (*) w2 w2

srcDeriv :: forall v1 e1 v2 e2. (Ord v1, Ord e1, Ord v2, Ord e2) =>
    ElasticGraph v1 e1 -> ElasticGraph v2 e2 ->
    FillingMap v1 e1 v2 e2 -> e1 -> Double
srcDeriv eg1@(g1,elast1) (g2,elast2) (fp,w2) e1 =
    let l2 = elasticMatrix elast2 # w2
        tens1 = tensions eg1 (g2,l2) fp `at` e1
    in -(tens1*tens1)/energy eg1 (g2,l2) fp

srcGradient :: forall v1 e1 v2 e2. (Ord v1, Ord e1, Ord v2, Ord e2) =>
    ElasticGraph v1 e1 -> ElasticGraph v2 e2 ->
    FillingMap v1 e1 v2 e2 -> Vec e1
srcGradient eg1@(g1,elast1) (g2,elast2) (fp,w2) =
    let l2 = elasticMatrix elast2 # w2
        tens1 = tensions eg1 (g2,l2) fp
        tens1sq = zipIMatrix (*) tens1 tens1
    in scale (-1/energy eg1 (g2,l2) fp) tens1sq
\end{code}

For a virtual endomorphism, a given edge appears in the target and
multiple times in the source. Add up all of these derivatives in order
to get the total gradient.

\begin{code}
endoGradient :: (Ord c, Ord v, Ord e) =>
                ElasticGraph v e -> VirtualEndo c v e ->
                FillingMap (v,c) (e,c) v e -> Vec e
endoGradient eg1@(g1,elast1) (g2,f2) fm =
    let elast2 = liftVec g2 elast1; eg2 = (g2,elast2)
        sg = srcGradient eg2 eg1 fm
        tg = tgtGradient eg2 eg1 fm
    in makeVec (edges g1) (unmakeVec tg ++
                           map (\((a,b),c) -> (a,c)) (unmakeVec sg))

endoGradient' :: (Ord c, Ord v, Ord e) =>
                 ElasticGraph v e -> VirtualEndo c v e -> (Double,Vec e)
endoGradient' eg ve = 
    let (fm, fill) = endoFilling eg ve
    in (fill,endoGradient eg ve fm)
\end{code}

\end{document}
