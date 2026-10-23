\documentclass[12pt]{amsart}
\input{preamble}
\input{defs}

\begin{document}

Take double branched cover of a sphere branched at four points to get     
a torus, and compute harmonic maps from the result. This is useful for  
computing extremal lengths on a 4-punctured sphere.

\begin{code}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
    FlexibleInstances, DeriveFunctor, ScopedTypeVariables
  #-}
module Torus where
import Mappable
import Graph
import GraphMap
import GraphCover
import GraphIO
import IMatrix
import Harmonic
import Util
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.List as List
import qualified Data.Char as Char
import Circle
import Text.Printf -- For utility use in browser
\end{code}

The four-punctured sphere, represented as a bouquet of three circles,    
and its torus cover.

\begin{code}
rose3 = readGraph "apPqQrR"
torus = readCover rose3 "p01 q01 r01"

type RoseV = SimpleV
type RoseE = SimpleE
type TorusV = CoverV
type TorusE = CoverE
\end{code}

\begin{code}
torusClass :: (Int,Int) -> GraphMap CoverV CoverE CircleV CircleE
torusClass (n,m) =
    let rep 0 e = []
        rep n e | n > 0 = replicate n (Forward e)
                | n < 0 = replicate (-n) (Backward e)
    in makeGraphMap torus circle
                     [(('p','0'), rep n      'a'),
                     (('p','1'), rep (-n)   'a'),
                     (('q','0'), rep m      'a'),
                     (('q','1'), rep (-m)   'a'),
                     (('r','0'), []),
                     (('r','1'), [])]
\end{code}

IN PROGRESS: Given a map to a 4-punctured sphere:
Lift to map to torus
Compose to get map to circle
End up with his function (taking energy, etc):

\begin{code}
elTorus :: forall v e. (Ord v, Ord e) =>
           ElasticGraph v e -> GraphMap v e RoseV RoseE -> (Int,Int) -> Double
elTorus (g,elast) f nm =
    let (glift, flift) = liftMap torus (g,f)
        elastlift = liftVec glift elast
        fliftnm = composeMap glift torus (torusClass nm) flift
    in findEnergy (glift,elastlift) circleL fliftnm
\end{code}

\end{document}
