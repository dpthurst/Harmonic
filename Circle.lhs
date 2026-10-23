\documentclass[12pt]{amsart}
\input{preamble}
\input{defs}

\begin{document}

Maps to the circle for computing the harmonic energy. This can be
speeded up by specializing: this is really a pure linear algebra
problem.

\begin{code}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
    FlexibleInstances, DeriveFunctor, ScopedTypeVariables
  #-}
module Circle where
import Graph
import GraphMap
import GraphCover
import GraphIO
import Harmonic

type CircleV = SimpleV
type CircleE = SimpleE
circle = readGraph "xaA"

circleMap :: forall v e. (Ord v, Ord e) =>
             Graph v e -> e -> GraphMap v e CircleV CircleE
circleMap g e = makeGraphMap g circle [(e,[Forward 'a'])]

circleL = lengthGraph circle [1]
\end{code}
\end{document}
