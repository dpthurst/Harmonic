\documentclass[12pt]{amsart}
\input{preamble}
\input{defs}

\begin{document}

TODO: Distinguish three kinds of maps.
\begin{itemize}
\item Simplicial maps, taking vertices to vertices.
\item Combinatorial maps, taking vertices to vertices or edges (not precisely)
\item Geometric maps, specifying where on an edge a vertex goes.
\end{itemize}

\begin{code}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
    FlexibleInstances, DeriveFunctor, ScopedTypeVariables,
    FlexibleContexts
  #-}
module GraphMap where
import Prelude hiding ((!), reverse, elem, lookup)
import qualified Data.Map as Map
import Data.Map (Map, (!), fromList, lookup)
import qualified Data.Set as Set
import Data.Set (Set, union)
import qualified Data.List as List

import Mappable
import Graph
import Util(extend)
\end{code}


Chains of edges.

TODO: Explain exactly what the chain of edges means.

\begin{code}
type Chain e = [Or e]
type ChainEnd v e = (Either v e, Chain e, Either v e)

isChainEnd :: forall v e. (Ord v, Ord e) => Graph v e -> ChainEnd v e -> Bool
isChainEnd g (s,es,t) =
    let srcs = tail (map (\x -> src (g,x)) es)
        tgts = init (map (\x -> tgt (g,x)) es)
        checkEnd :: Either v e -> Or e -> Bool
        checkEnd (Left v)  e  = v == src (g,e)
        checkEnd (Right e) e' = e == unOr e'
    in if (null es) then s == t
       else and (zipWith (==) srcs tgts) &&
            checkEnd s (head es) && checkEnd t (reverse (last es))

joinChainEnd :: (Ord v, Ord e) => ChainEnd v e -> ChainEnd v e -> ChainEnd v e
joinChainEnd (p1,es1,p2) (p2',es2,p3) | p2 /= p2'
    = error "joinChain: endpoints do not match"
joinChainEnd (p1,es1,Left v2)  (_,es2,p3) = (p1, cancel (es1++es2), p3)
joinChainEnd (p1,es1,Right e2) (_,es2,p3) =
    (p1, if null es1 || null es2 then es1 ++ es2
         else if last es1 == head es2 then es1 ++ tail es2
         else es1 ++ es2,
    p3)

concatChains :: (Ord v, Ord e) => [ChainEnd v e] -> Chain e
concatChains [] = []
concatChains l = let (_,ch,_) = foldl1 joinChainEnd l in ch

instance (Ord v, Ord e) => Arrow (Graph v e, Chain e) v v where
    src (g, []) = error "src: Can't take source of empty chain"
    src (g, ch) = src (g, head ch)
    tgt (g, []) = error "tgt: Can't take target of empty chain"
    tgt (g, ch) = tgt (g, last ch)
\end{code}

Graph maps $f$ from $\Gamma_1$ to $\Gamma_2$, up to homotopy sliding
vertices within an edge. Each vertex maps either to a vertex or to the
interior of an edge. Each edge of $\Gamma_1$ then maps to a chain of
edges of $\Gamma_2$.

\begin{code}
type VertexMap v1 e1 v2 e2 = Map v1 (Either v2 e2)
type EdgeMap   v1 e1 v2 e2 = Map e1 (Chain e2)
data GraphMap  v1 e1 v2 e2 =
    GM { vm :: VertexMap v1 e1 v2 e2, em :: EdgeMap v1 e1 v2 e2 }
              deriving (Show, Read, Eq, Ord)

-- Overlaps currently forbid more instances here
instance (Ord e1) => Mappable (GraphMap v1 e1 v2 e2) (Or e1) [Or e2] where
  f # (Forward e)  = (em f)!e
  f # (Backward e) = reverse ((em f)!e)

mapChainEnd :: (Ord v1, Ord e1, Ord v2, Ord e2) =>
               Graph v1 e1 ->
               GraphMap v1 e1 v2 e2 -> Or e1 -> ChainEnd v2 e2
mapChainEnd s f e = (vm f ! src (s,e), f # e, vm f ! tgt (s, e))

isV, isE :: (Ord v1) => GraphMap v1 e1 v2 e2 -> v1 -> Bool
isV f v = case (vm f)!v of { Left _ -> True;  Right _ -> False }
isE f v = case (vm f)!v of { Left _ -> False; Right _ -> True  }
\end{code}

A more refined version of \texttt{makeGraphMap}, one that deduces the
mapping on the vertices from the mapping on the edges. (This only
works for simplicial maps, where each vertex maps to a vertex.) For
this to work, there needs to be at least one non-constant edge in each     
connected component. This is a lousy implementation of union-find.

Should do some checking that this constructs a valid GraphMap.

\begin{code}
makeGraphMap :: forall v1 e1 v2 e2. (Ord v1, Ord e1, Ord v2, Ord e2) =>
                Graph v1 e1 -> Graph v2 e2 ->
                [(e1, Chain e2)] -> GraphMap v1 e1 v2 e2
makeGraphMap g1 g2 emap0 =
    let emap = filter (not . null . snd) emap0
        empt0 = (edges g1) List.\\ (map fst emap)
        empt = map Forward empt0 ++ map Backward empt0
        vmap0 :: Map v1 v2
        vmap0 = Map.fromList (concatMap (\(e1, ch2) -> [(src (g1,e1), src (g2,ch2)), (tgt (g1,e1), tgt (g2,ch2))]) emap)
        iter :: (Map v1 v2, [Or e1]) -> (Map v1 v2, [Or e1])
        iter (vmap, empt) =
            let empt1 = filter (\e -> Map.notMember (tgt (g1,e)) vmap) empt
                (empt2,empt3) = List.partition (\e -> Map.member (src (g1,e)) vmap) empt1
                vmap1 = Map.union vmap
                          (Map.fromList [(tgt(g1,e), vmap!src(g1,e)) | e <- empt2])
            in (vmap1,empt3)
        vmap = fst $ head $ dropWhile (not.null.snd) $ iterate iter (vmap0,empt)
    in GM (Map.map Left vmap) (Map.fromList (extend [] (edges g1) emap))
\end{code}

Versions of the identity. Allow renaming vertices and edges.

\begin{code}
idMap :: (Ord v, Ord e) => Graph v e -> GraphMap v e v e
idMap g = makeGraphMap g g [(e, [Forward e]) | e <- edges g]

idVEMap :: (Ord v, Ord e, Ord v', Ord e') =>
           (v -> v') -> (e -> e') -> Graph v e
                     -> Graph v' e' -> GraphMap v e v' e'
idVEMap f1 f2 g1 g2 =
    makeGraphMap g1 g2 [(e, [Forward (f2 e)]) | e <- edges g1]
\end{code}

Verify that we have a valid GraphMap. For each edge $e$ of $\Gamma_1$,
$f(e)$ must be a chain. Furthermore, for each vertex $v$ of
$\Gamma_1$,
\begin{itemize}
\item if $f(v)$ is a vertex, then for each edge $e$ incident to $v$,
  $f(e)$ is a chain that must start at $f(v)$; and
\item if $f(v)$ is an edge, then for each edge $e$ incident to $v$,
  $f(e)$ must start with $f(v)$, either forward or backward.
\end{itemize}

\begin{code}
-- There are some unsafe lookups here
prop_graphMapEdge :: (Ord v1, Ord e1, Ord v2, Ord e2) =>
                     Graph v1 e1 -> Graph v2 e2 ->
                     GraphMap v1 e1 v2 e2 -> e1 -> Bool
prop_graphMapEdge s t f e =
    case lookup e (em f) of
      Nothing -> False
      Just c  -> isChainEnd t ((vm f)!(src (s,e)), c, (vm f)!(tgt (s,e)))


prop_graphMap :: (Ord v1, Ord e1, Ord v2, Ord e2) =>
                 Graph v1 e1 -> Graph v2 e2 ->
                 GraphMap v1 e1 v2 e2 ->  Bool
prop_graphMap s t f =
  all (\e -> prop_graphMapEdge s t f e) (edges s)
\end{code}

Compose two GraphMap's. This is not actually a well-defined operation,
as the equivalence relation we quotient by is not preserved by
composition. We punt and only support composition where the first map
takes vertices to vertices.

\begin{code}
composeMap :: forall v1 e1 v2 e2 v3 e3. 
              (Ord v1, Ord e1, Ord v2, Ord e2, Ord v3, Ord e3) =>
              Graph v1 e1 -> Graph v2 e2 ->
              GraphMap v2 e2 v3 e3 -> GraphMap v1 e1 v2 e2
                -> GraphMap v1 e1 v3 e3
composeMap s t g f =
    let vm' :: VertexMap v1 e1 v3 e3
        vm' = Map.fromList [ (v1, ev3) |
                             (v1, Left v2) <- Map.toList (vm f),
                             let ev3 = (vm g)!v2 ]
        em' = Map.fromList [ (e1, ch3) |
                             (e1, ch2) <- Map.toList (em f),
                             let ch3 = concatChains(map (mapChainEnd t g) ch2) ]
    in GM vm' em'

instance (Ord v1, Ord e1, Ord v2, Ord e2, Ord v3, Ord e3) =>
            Mappable (Graph v2 e2,GraphMap v2 e2 v3 e3)
                     (Graph v1 e1,GraphMap v1 e1 v2 e2)
                     (Graph v1 e1,GraphMap v1 e1 v3 e3) where
     (t,f) # (s,g) = (s,composeMap s t f g)
\end{code}

We may want to modify a graph map by a more general homotopy (beyond
just sliding within an edge). First, we can ``pull'' a vertex of
$\Gamma_1$ from a vertex of $\Gamma_2$ onto a neighboring edge of
$\Gamma_2$.

\begin{code}
pullVertex :: forall v1 e1 v2 e2. (Ord v1, Ord e1, Ord v2, Ord e2) =>
              Graph v1 e1 -> Graph v2 e2 -> GraphMap v1 e1 v2 e2 ->
              (v1, Or e2) -> GraphMap v1 e1 v2 e2
pullVertex s t f (v1,e2)
  | (vm f)!v1 == Left (src (t,e2)) =
    let vm' = Map.insert v1 (Right (unOr e2)) (vm f)
        updateEdge :: Or e1 -> Chain e2 -> Chain e2
        updateEdge e1 c2 =
            case c2 of
              [e2']     | e2' == e2 && (vm f)!(tgt (s,e1)) == Right (unOr e2)
                                    -> []
              v@(e2':_) | e2' == e2 -> v
              v                     -> (reverse e2):v
        em' = updateEdges s t f v1 updateEdge
    in GM vm' em'
  | otherwise = error "pullVertex: Precondition not satisfied"
\end{code}

We can also ``push'' a vertex of $\Gamma_1$ from an edge of $\Gamma_2$
onto a neighboring vertex.

\begin{code}
pushVertex :: forall v1 e1 v2 e2. (Ord v1, Ord e1, Ord v2, Ord e2) =>
              Graph v1 e1 -> Graph v2 e2 -> GraphMap v1 e1 v2 e2 ->
              (v1, Or e2) -> GraphMap v1 e1 v2 e2
pushVertex s t f (v1,e2)
  | (vm f)!v1 == Right (unOr e2) =
    let vm' = Map.insert v1 (Left (tgt (t,e2))) (vm f)
        updateEdge :: Or e1 -> Chain e2 -> Chain e2
        updateEdge e1 c2 =
            case c2 of
              []                     -> [reverse e2]
              (e2':c')   | e2' == e2 -> c'
              c'@(e2':_) | e2' == reverse e2 -> c'
        em' = updateEdges s t f v1 updateEdge
    in GM vm' em'
  | otherwise = error "pushVertex: Precondition not satisfied"
\end{code}

An internal function used in both cases: Just update the edges.
I'm a little nervous about doing the updates iteratively like this. Is
there some cleaner way to take care of both endpoints?

\begin{code}
updateEdges :: forall v1 e1 v2 e2. (Ord v1, Ord e1, Ord v2, Ord e2) =>
               Graph v1 e1 -> Graph v2 e2 -> GraphMap v1 e1 v2 e2 ->
               v1 -> (Or e1 -> Chain e2 -> Chain e2) -> EdgeMap v1 e1 v2 e2
updateEdges s t f v1 upd =
    let upd' :: Or e1 -> Chain e2 -> Chain e2
        upd' e@(Forward  _) c = upd e c
        upd' e@(Backward _) c = reverse (upd e (reverse c))
        oneUpd :: EdgeMap v1 e1 v2 e2 -> Or e1 -> EdgeMap v1 e1 v2 e2
        oneUpd em e = Map.adjust (upd' e) (unOr e) em
    in foldl oneUpd (em f) (neighbors s v1)
\end{code}

One special case: ``snap'' a graph so that vertices map to
vertices. This is useful before using \texttt{composeMap}.

\begin{code}
snapMap :: forall v1 e1 v2 e2. (Ord v1, Ord e1, Ord v2, Ord e2) =>
           Graph v1 e1 -> Graph v2 e2 -> GraphMap v1 e1 v2 e2
           -> GraphMap v1 e1 v2 e2
snapMap s t f =
    let ves :: [(v1,Or e2)]
        ves = [ (v,Backward e) | (v, Right e) <- Map.toList $ vm f ]
    in foldl (pushVertex s t) f ves
\end{code}

\end{document}
