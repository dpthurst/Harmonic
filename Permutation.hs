{-# LANGUAGE ExistentialQuantification #-}
module Permutation where
import Test.QuickCheck
import Data.List(nub,sort,(\\),cycle,tails)
import Data.List.Split(splitOn,chop)
import Util(fi,TestCase(TestCase))
import CyclicList

newtype Permutation a = Permutation {permPairs :: [(a,a)]}
     deriving (Show, Read)

emptyP :: Permutation a -> Bool
emptyP = null . permPairs

-- Expand a permutation by the identity to a larger domain/range
extendP :: (Eq a) => [a] -> Permutation a -> Permutation a
extendP dom p =
    let dom' = dom \\ (map fst (permPairs p))
    in Permutation (permPairs p ++ map (\x -> (x,x)) dom')

idP :: (Eq a) => [a] -> Permutation a
idP dom = Permutation [(a,a) | a <- dom]

isIdP :: (Eq a) => Permutation a -> Bool
isIdP p = all (uncurry (==)) (permPairs p)

imageP :: (Eq a) => a -> Permutation a -> a
imageP x p =
  case lookup x (permPairs p) of
    Just y -> y
    Nothing -> error "imageP: element not in domain"

inverseP :: Permutation a -> Permutation a
inverseP (Permutation p) = Permutation (map (\(x,y) -> (y,x)) p)

canonP :: (Ord a) => Permutation a -> Permutation a
canonP (Permutation p) = Permutation (sort p)

composeP :: (Eq a) => Permutation a -> Permutation a -> Permutation a
composeP p0 p1 = Permutation [(a,c) | (a,b) <- permPairs p0, (b',c) <- permPairs p1, b == b']

instance (Ord a) => Eq (Permutation a) where
  p0 == p1 = permPairs (canonP p0) == permPairs (canonP p1)

instance Functor Permutation where
    fmap f (Permutation p) = Permutation (map (\(x,y) -> (f x, f y)) p)

type Cycles a = [CyclicList a]

permCycles :: (Eq a) => Permutation a -> Cycles a
permCycles p = case permPairs p of
  [] -> []
  (x0,_):_ ->
    let next x = imageP x p
        l0 = unfoldCyclic next x0
        p0 = cyclePerm [l0]
        p' = Permutation (permPairs p \\ permPairs p0)
        c' = permCycles p'
    in l0:c'

cyclePerm :: Cycles a -> Permutation a
cyclePerm c =
  let aux :: [a] -> [(a,a)]
      aux l = map (\(x:y:_) -> (x,y)) (take (length l) (tails (cycle l)))
  in Permutation (concatMap aux c)

numCycles :: (Eq a) => Permutation a -> Int
numCycles p = length (permCycles p)

readSPerm :: String -> Permutation Char
readSPerm s = cyclePerm (splitOn "|" s)

permutationOf :: [a] -> Gen (Permutation a)
permutationOf [] = return (Permutation [])
permutationOf l = do
    i <- choose (0, length l - 1)
    Permutation p' <- permutationOf (take i l ++ drop (i+1) l)
    return (Permutation (zip l (l!!i:map snd p')))

instance (Num a, Enum a, Arbitrary a) => Arbitrary (Permutation a) where
    arbitrary = sized (\n -> permutationOf [0..(fi n-1)])

instance (CoArbitrary a) => CoArbitrary (Permutation a) where
    coarbitrary (Permutation p) = coarbitrary p

prop_inrange, prop_distinct, prop_linverse, prop_rinverse, prop_cycles ::
    Permutation Int -> Bool
prop_inrange (Permutation p) = all (`elem` p0) p1
  where p0 = map fst p; p1 = map snd p
prop_distinct (Permutation p) = sort p0 == sort p1
  where p0 = map fst p; p1 = map snd p
prop_linverse p = isIdP (inverseP p `composeP` p)
prop_rinverse p = isIdP (p `composeP` inverseP p)
prop_cycles p = cyclePerm (permCycles p) == p

permutationProps =
    [TestCase "Permutation.inrange" prop_inrange,
     TestCase "Permutation.distinct" prop_distinct,
     TestCase "Permutation.linverse" prop_linverse,
     TestCase "Permutation.rinverse" prop_rinverse,
     TestCase "Permutation.cycles" prop_cycles]
