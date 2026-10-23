\documentclass[12pt]{amsart}
\input{preamble}
\input{defs}

\begin{document}

\begin{code}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
    FlexibleInstances, DeriveFunctor, ScopedTypeVariables
  #-}
module Examples where
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
import Text.Printf -- For utility use in browser
\end{code}
\section{Double theta to theta example}

The graphs.
\begin{code}
theta :: Graph SimpleV SimpleE
theta = readGraph "apqr bRQP"

type Deg2 = Char
dtheta :: GraphCover Deg2 SimpleV SimpleE
dtheta = readCover theta "q01"

thetaMap :: GraphMap (SimpleV,Deg2) (SimpleE,Deg2) SimpleV SimpleE
thetaMap = readCMap dtheta theta "p0P p1r r0Q r1q"
\end{code}

Finding harmonic representatives
\begin{code}
thetaLengths :: Lengths SimpleE
thetaLengths = edgeVec theta [2,1,3]
thetaL = (theta, thetaLengths)

thetaElastics = edgeVec theta [4,1,4]
thetaE = (theta, thetaElastics)
dthetaElastics = liftVec dtheta thetaElastics
dthetaE = (dtheta, dthetaElastics)

thetaHarmonic = findHarmonic dthetaE thetaL thetaMap
\end{code}

Finding filling representative.
\begin{code}
thetaEndo = (dtheta, thetaMap)
thetaFilling = endoFilling thetaE thetaEndo
\end{code}
Iterating.
\begin{code}
thetaEndos = iterateEndo (theta,thetaEndo)
thetaSFs = endoSFs (theta,thetaEndo)
\end{code}
An example, for lengths of $(4,1,4)$.
\begin{code}
thetaExamp = thetaSFs [4,1,4]
\end{code}

\section{Bouquet example}
This can be obtained from contracting the theta example.
\begin{code}
-- bouquet n = makeGraph [(k, 'X', 'X') | k <- [0..n-1] ]
bouquet = readEndo "XaABb" "a01 b01" "a0A a1b"
bouquetSFs elast = endoSFs bouquet elast
\end{code}
\section{Barbell}
Again, this is homotopy equivalent to the same example above
\begin{code}
barbell = readEndo "aqpP bQRr" "p01 r01" "p1P p0qrQ"
barbellSFs elast = endoSFs barbell elast
\end{code}
\section{Slit Lattés example}
This was suggested by Kevin Pilgrim
\begin{code}
slitLattes = readEndo "apqr bRQP" "r012 q01" "q1P p1Q p2q q2r"
slitLattesSFs elast = endoSFs slitLattes elast
\end{code}

\section{Quadratic Sierpinski Carpet}

This map is the unique quadratic Sierpinski carpet with 4     
post-critical points. In Kevin's census, it is \#3.2. Graphs shown in
\texttt{2021-11-10 T0}, and doubtless earlier.
\begin{code}
sierp@(sierp0,sierpEndo) =
    readEndo "atpq bTrs cuQP dUSR" "r01" "s0p p0q q0trU q1R p1S s1S r0t r1U"
sierpSFs = endoSFs sierp
sierpCircMap = circleMap sierp0 't'
\end{code}

Here are the current best parameters for stretch factor at the first
iterate. The actual $\ASF$ seems to be about $0.94$. This one gives
1.110 at first iteration, so needs to iterate to give a
certificate. Because $\ASF$ is so close to~$1$, these parameters need
quite a few iterates to get close.
\begin{code}
sierpElast = [189.0,211.0,68.8,170.0,1.0,1.0]
\end{code}

\section{Example from Hamal's birthday conference}
Here is the example that was suggested from the audience during     
``Dynamical Developments'', a conference in Bremen in honor of John            
Hamal Hubbard's 70th birthday. It is a twist of the rabbit.

First, the graph and its cover.

\begin{code}
rabbitRose = readGraph "xaAbBcC"
rabbitCover = readCover rabbitRose "a01"
\end{code}
Here is the map that gives the untwisted rabbit.
\begin{code}
rabbitMap = readCMap rabbitCover rabbitRose "b0a c0b b0c"
\end{code}
And here is the twisted rabbit. The suggested Dehn twist amounts to
conjugating the image of the loops $b_0$ and $c_0$ by $ab$.
\begin{code}
twistedRabbitMap = readCMap rabbitCover rabbitRose "b0Bab c0BAbab b1c"
twistedRabbitSFs = endoSFs (rabbitRose,(rabbitCover,twistedRabbitMap))
\end{code}
Because this branched cover does \emph{not} satisfy the condition that
every cycle in $P$ is a critical cycle, the main theorem does not apply:
you will never see the exponential decay that would guarantee that the
map is rational.

\section{The Grigorchuk group}
This is just here as an example of a graph endomorphism that does not
come from a rational map. This particular example has $\ASF = 1$,
since the generators are all torsion inside of the iterated monodromy
group. (To get a better example we should work with orbifolds.)
\begin{code}
grigor = readEndo "apPqQrRsS" "p01" "q1r r1s s1q q0p r0p"
grigorSFs elast = endoSFs grigor elast
\end{code}

\section{Tetrahedral subdivision}
From \url{http://www.math.univ-toulouse.fr/~buff/Symmetries/Patterns.html}

\begin{code}
tetrahedral = readEndo "1pqr 2Pts 3QSu 4RUT" "sac tab ubc" "ubR qcq scp rbU tbT pas"
\end{code}

\section{Barycentric subdivision}
The branched cover that implements successive barycentric subdivision of
triangles (doubled). One of Kevin's favorite examples, and subject of
the 2017 REU. See \texttt{2017-01-18\#2} for graph pictures.

\begin{code}
barycentric = readEndo "0pqr 1RQP" "rabc|def qad|bf|ce"
                "qaq qfQ qbr qeR qcp qdP"
\end{code}

For the barycentric subdivision, the optimizing target length graph is
always the theta graph of length 1 (by symmetries). Compute those
optimizing maps.

\begin{code}
thetaB = fst barycentric
thetaStd = (thetaB, edgeVec thetaB [1.0,1.0,1.0])
baryEndos = iterateEndo barycentric
baryHarmonic (gn,fn) =
  let gnElast = (gn, edgeVec gn (repeat 1.0))
      h = findHarmonic gnElast thetaStd fn
  in (h, energy gnElast thetaStd h)
baryHarmonics = map baryHarmonic baryEndos
baryEnergies = map snd baryHarmonics
\end{code}

\section{Blown-up Lattès example}

There is a degree 2 Lattès map with orbifold the right-angled
triangle. If you blow up that map along one edge, you get a degree 3
hyperbolic map.

\begin{code}
blownUpLattes = readEndo "0abc 1CBA" "axyz cxy" "cxb cyA axc bzc czc"
\end{code}

\end{document}

\section{Census examples}

\subsection{Census \#53}

\begin{code}
ccensus53 = readEndo "xRrg yGBb" "b012 r12" "g2r g0G r0b"
-- elast lengths in order bgr
ccensus53elasts = [24, 3 + sqrt 105, 18+2*sqrt 105]
\end{code}
