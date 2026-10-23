\documentclass[12pt]{amsart}
\input{preamble}
\input{defs}

\begin{document}

\begin{code}
{-# LANGUAGE ExistentialQuantification, TupleSections #-}
module Util where
import Data.List(maximumBy,(\\))
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Test.QuickCheck
\end{code}

Machine precision of Double, so $1 + \epsilon \ne \epsilon$.

\begin{code}
eps :: Double
eps = 2.22044604925031e-16
eps4 = 4*eps
\end{code}

Check triangle inequalities. Allow some slack.

\begin{code}
checkTriangle :: (Ord a) => [(a,Double)] -> Maybe a
checkTriangle [] = Nothing
checkTriangle vs =
    let m = Map.fromListWith (+) vs
        s = sum (Map.elems m)
        (kMax,vMax) = maximumBy (\x y -> compare (snd x) (snd y)) (Map.assocs m)
    in if (vMax*2*(1-eps4) > s) then Just kMax else Nothing
\end{code}

Looping a Maybe call.

\begin{code}
iterateMaybe :: (a -> Maybe a) -> Maybe a -> [a]
iterateMaybe f Nothing = []
iterateMaybe f (Just a) = a:iterateMaybe f (f a)
\end{code}

\begin{code}
allEqual :: (Eq a) => [a] -> Bool
allEqual l = and (zipWith (==) l (tail l))

mapEV :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapEV f g (Left a) = Left (f a)
mapEV f g (Right b) = Right (g b)
\end{code}

Continued fractions for rational approximations.
\begin{code}
cfrac :: Double -> [Integer]
cfrac x | abs x < eps4 = []
cfrac v = floor v:cfrac (1/(v - (fromInteger (floor v))))

makefrac :: [Integer] -> Rational
makefrac [a] = fromInteger a
makefrac (a:as) = fromInteger a + 1/makefrac as
\end{code}

Utility for examining stretch factors of results.
\begin{code}
nthRoots :: [Double] -> [Double]
nthRoots l = zipWith (\z n -> z ** (1/n)) (tail l) [1..]
\end{code}

Extend a partial function, represented as (key,value) pairs, to       
include default values on the given domain.

Also another version for maps. Really I should be using the second one     
always.
\begin{code}
extend :: (Ord a) => b -> [a] -> [(a,b)] -> [(a,b)]
extend dflt domain pairs = pairs ++ map (, dflt) (domain \\ (map fst pairs))

extendMap :: (Ord a) => b -> Set a -> Map a b -> Map a b
extendMap dflt domain m0 = m0 `Map.union` (Map.fromSet (const dflt) domain)
\end{code}

Find maximum value where a proposition is true by bisection.

\begin{code}
bisect :: Double -> (Double -> Bool) -> Double -> Double -> Double
bisect prec p min max =
  if (max - min <= prec) then mid
  else if p mid then bisect prec p mid max
  else bisect prec p min mid
  where mid = (min + max)/2
\end{code}

Summing powers, for estimating p-norms

\begin{code}
zetasum :: [Double] -> Double -> Double
zetasum l s = sum (map (** s) l)
\end{code}

Miscellaneous stuff.
\begin{code}
{-# INLINE fi #-}
fi x = fromInteger (toInteger x)

data TestCase = forall prop. (Testable prop) => TestCase String prop
\end{code}

\end{document}
