module CyclicList where
import Data.List(elemIndex, inits, tails)

-- A list considered cyclically
type CyclicList a = [a]

-- Find the specified element in the cyclic list, and return the
-- remainder of the cycle (which is an ordinary list) if found.
cyclicFind :: (Eq a) => a -> CyclicList a -> Maybe [a]
cyclicFind x l = do
  ix <- elemIndex x l
  return (drop (ix+1) l ++ take ix l)

-- This could probably be best written using 'cycle' from the Prelude
cyclicNext, cyclicPrev :: (Eq a) => a -> CyclicList a -> a
cyclicNext x l@(l0:_) = cyclicNext' l where
  cyclicNext' (x0:xs) | x0 == x   = case xs of { [] -> l0; (x1:_) -> x1 }
                      | otherwise = cyclicNext' xs
  cyclicNext' [] = error "cyclicNext: element not found"

cyclicPrev x l = cyclicNext x (reverse l)

rotations :: CyclicList a -> [CyclicList a]
rotations l = tail (zipWith (++) (tails l) (inits l))

rotate :: Int -> CyclicList a -> CyclicList a
rotate k [] = []
rotate k l =
  let n = length l
      k' = k `mod` n
  in (drop k' l) ++ (take k' l)

canonicalPos :: (Ord a) => CyclicList a -> CyclicList a
canonicalPos l = minimum (rotations l)

isCanonical :: (Ord a) => CyclicList a -> Bool
isCanonical [] = False
isCanonical l = all (>= l) (rotations l)

unfoldCyclic :: (Eq a) => (a -> a) -> a -> CyclicList a
unfoldCyclic f i = i:takeWhile (/= i) (iterate f (f i))
