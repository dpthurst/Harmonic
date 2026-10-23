\documentclass[12pt]{amsart}
\input{preamble}
\input{defs}

\begin{document}

\begin{code}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
    FlexibleInstances, DeriveFunctor, ScopedTypeVariables
  #-}
module GraphIO where
import qualified Data.Char as Char
import Data.List(sort,group)
import Data.List.Split(chunksOf)

import Graph
import GraphMap
import GraphCover
import Permutation(readSPerm)
import Util(extend)

type SimpleV = Char
type SimpleE = Char
type SimpleGraph = Graph SimpleV SimpleE
type SimpleMap = GraphMap SimpleV SimpleE SimpleV SimpleE

readV :: Char -> SimpleV
readV = id

readE :: Char -> Or SimpleE
readE c | Char.isLower c = Forward c
        | Char.isUpper c = Backward (Char.toLower c)
         

readFlower :: String -> (SimpleV, [Or SimpleE])
readFlower (v:es) = (readV v, map readE es)

readGraph :: String -> SimpleGraph
readGraph s = flowerGraph (map readFlower (words s))

readMap ::
    SimpleGraph -> SimpleGraph -> String -> SimpleMap
readMap g1 g2 se =
    let getE (e1:es) = (unOr (readE e1), map readE es)
    in makeGraphMap g1 g2 (map getE (words se))

type Cover = Char
type CoverV = (SimpleV, Cover)
type CoverE = (SimpleE, Cover)
type CoverGraph = GraphCover Cover SimpleV SimpleE
type CoverMap = GraphMap CoverV CoverE SimpleV SimpleE

readCV :: String -> CoverV
readCV [v,c] = (v,c)

readCE :: String -> Or CoverE
readCE [e,c] | Char.isLower e = Forward (e,c)
             | Char.isUpper e = Backward (Char.toLower e,c)

readCFlower :: String -> (CoverV, [Or CoverE])
readCFlower s = case (chunksOf 2 s) of (v:es) -> (readCV v, map readCE es)

readCGraph :: String -> CoverGraph
readCGraph s = flowerGraph (map readCFlower (words s))

readCover :: SimpleGraph -> String -> CoverGraph
readCover g sp =
    let -- Domain is all letters appearing in any permutation 
        sps = words sp
        dom = filter (/='|') $ map head $ group $ sort $ concat $ map tail $ sps
        readP (e:p) = (e, readSPerm p)
    in permCover g dom (map readP sps)

readCMap :: CoverGraph -> SimpleGraph -> String -> CoverMap
readCMap g1 g2 se =
    let getE (e1:c:es) = (unOr (readCE [e1,c]), map readE es)
    in makeGraphMap g1 g2 (map getE (words se))

type CoversV = (SimpleV, [Cover])
type CoversE = (SimpleE, [Cover])

readCsV :: String -> CoversV
readCsV (v:cs) = (v,cs)

readCsE :: String -> Or CoversE
readCsE (e:cs) | Char.isLower e = Forward (e,cs)
               | Char.isUpper e = Backward (Char.toLower e, cs)

readCsFlower :: String -> (CoversV, [Or CoversE])
readCsFlower s = case words s of (v:es) -> (readCsV v, map readCsE es)

readCsGraph :: String -> Graph CoversV CoversE
readCsGraph s = flowerGraph (map readCsFlower (lines s))

type SimpleEndo = VirtualEndo Cover SimpleV SimpleE

readEndo :: String -> String -> String -> (SimpleGraph, SimpleEndo)
readEndo gr cov mp =
    let g0 = readGraph gr
        g1 = readCover g0 cov
        gmap = readCMap g1 g0 mp
    in (g0, (g1,gmap))
\end{code}
\end{document}
