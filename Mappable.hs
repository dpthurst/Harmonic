{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Mappable where
import Prelude hiding (reverse)
import qualified Data.List as List

infixr 9 #

class Mappable f a b | f a -> b where
    (#) :: f -> a -> b

class Arrow f a b | f -> a, f -> b where
    src :: f -> a
    tgt :: f -> b

class Reversible a where
    reverse :: a -> a

instance Reversible a => Reversible [a] where
    reverse l = List.reverse (map reverse l)

cancel :: (Reversible a, Eq a) => [a] -> [a]
cancel l = cancel' [] l where
    cancel' (a:as) (b:bs) | a == b = cancel' as bs
    cancel' as (b:bs)              = cancel' ((reverse b):as) bs
    cancel' as []                  = reverse as
