{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
    FlexibleInstances, DeriveFunctor
  #-}
module IMatrix where
import Prelude hiding ((<>))
import qualified Data.Array as Array
import Data.Array(Array, (!))
import Data.List(elemIndex)
import Numeric.LinearAlgebra
import Control.Monad(liftM2)

import Mappable

data One = One deriving (Eq, Ord, Show, Read)

-- An "indexed matrix", that knows a little what its source and destination are
data IMatrix s t = IMatrix [s] [t] (Maybe (Matrix Double))
                    deriving (Eq, Show, Read)

type Vec t = IMatrix One t

instance Arrow (IMatrix s t) [s] [t] where
    src (IMatrix s _ _) = s
    tgt (IMatrix _ t _) = t

instance (Eq t) => Mappable (IMatrix t u) (IMatrix s t) (IMatrix s u) where
    (IMatrix _ [] Nothing) # (IMatrix s _ _) = IMatrix s [] Nothing
    (IMatrix _ u _) # (IMatrix [] _ Nothing) = IMatrix [] u Nothing
    (IMatrix [] u Nothing) # (IMatrix s [] Nothing) = IMatrix s u (Just 0)
    (IMatrix t1 u m1) # (IMatrix s t2 m2)
        | t1 == t2 = IMatrix s u (liftM2 (<>) m1 m2)
        | otherwise = error "IMatrix.#: index sets do not match"

instance (Eq s, Eq t) => Mappable (IMatrix s t) (s,t) Double where
    (IMatrix ss ts (Just m)) # (s,t) =
        let ix1 s = case (elemIndex s ss) of (Just i) -> i
            ix2 t = case (elemIndex t ts) of (Just j) -> j
        in m `atIndex` (ix2 t, ix1 s)

at :: (Eq t) => Vec t -> t -> Double
v `at` t = v # (One, t)

makeIMatrix :: (Ord s, Ord t) => [s] -> [t] -> [((s,t), Double)] -> IMatrix s t
makeIMatrix ss ts es0 =
    let n1 = length ss; n2 = length ts
        ix1 s = case (elemIndex s ss) of (Just i) -> i
        ix2 t = case (elemIndex t ts) of (Just j) -> j
        ix (s,t) = (ix2 t, ix1 s)
        es = map (\(ij,v) -> (ix ij, v)) es0
        m = fromArray2D $ Array.accumArray (+) 0 ((0,0),(n2-1,n1-1)) es
    in IMatrix ss ts (if n1 == 0 || n2 == 0 then Nothing else Just m)

makeVec :: (Ord t) => [t] -> [(t, Double)] -> Vec t
makeVec ts es = makeIMatrix [One] ts (map (\(t,v) -> ((One,t),v)) es)

makeOneVec :: (Ord t) => [t] -> Vec t
makeOneVec ts = makeVec ts (zip ts (repeat 1))

unmakeVec :: (Ord t) => Vec t -> [(t, Double)]
unmakeVec (IMatrix _ _ Nothing) = []
unmakeVec (IMatrix _ ts (Just m)) = zip ts (toList $ head $ toColumns m)

-- Really should be using dual vector spaces around here  
transpose :: IMatrix s t -> IMatrix t s
transpose (IMatrix s t m) = IMatrix t s (fmap tr m)

diagonal :: Vec t -> IMatrix t t
diagonal (IMatrix _ t Nothing) = IMatrix t t Nothing
diagonal (IMatrix _ t (Just v)) =
    let m = diag (flatten v)
    in IMatrix t t (Just m)

mapIMatrix :: (Double -> Double) -> IMatrix s t -> IMatrix s t
mapIMatrix f (IMatrix s t Nothing) = IMatrix s t Nothing
mapIMatrix f (IMatrix s t (Just m)) = IMatrix s t (Just (cmap f m))

zipIMatrix :: (Double -> Double -> Double) -> IMatrix s t -> IMatrix s t -> IMatrix s t
zipIMatrix f (IMatrix s t Nothing) _ = IMatrix s t Nothing
zipIMatrix f _ (IMatrix s t Nothing) = IMatrix s t Nothing
zipIMatrix f (IMatrix ss ts (Just m)) (IMatrix ss' ts' (Just n)) =
    IMatrix ss ts (Just (zipMatrix f m n))

zipMatrix :: (Element a, Element b, Element c) =>
             (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
zipMatrix f m n = fromLists $ zipWith (zipWith f) (toLists m) (toLists n)

solve :: (Eq t) => IMatrix s t -> IMatrix u t -> IMatrix u s
solve a b | tgt a /= tgt b = error "IMatrix.solve: index sets do not match"
solve (IMatrix s t a) (IMatrix u _ b) =
    IMatrix u s (liftM2 linearSolveSVD a b)

instance (Eq s, Eq t) => Num (IMatrix s t) where
    (IMatrix s1 t1 m1) + (IMatrix s2 t2 m2)
        | s1 == s2 && t1 == t2 = IMatrix s1 t1 (liftM2 (+) m1 m2)
        | otherwise = error "IMatrix.+: indices don't match"
    negate (IMatrix s t m) = IMatrix s t (fmap negate m)
    fromInteger _ = error "IMatrix.fromInteger: Not yet implemented"
    _ * _ = error "IMatrix.*: Not implemented"
    signum _ = error "IMatrix.signum: Not implemented"
    abs _ = error "IMatrix.abs: Not implemented"

norm :: IMatrix s t -> Double
norm (IMatrix s t Nothing) = 0
norm (IMatrix s t (Just m)) = norm_2 m

scale :: Double -> IMatrix s t -> IMatrix s t
scale _ m@(IMatrix s t Nothing) = m
scale v (IMatrix s t (Just m)) = IMatrix s t (Just (Numeric.LinearAlgebra.scale v m))
