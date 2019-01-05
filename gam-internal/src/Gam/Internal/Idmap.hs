module Gam.Internal.Idmap where

import Gam.Internal.Prelude

import Data.Foldable

import qualified Data.IntMap.Strict as IntMap

data Idmap a
  = Idmap Int (IntMap a)
  deriving stock (Show)

instance Foldable Idmap where
  fold (Idmap _ m) = fold m
  foldMap f (Idmap _ m) = foldMap f m
  foldr f z (Idmap _ m) = foldr f z m
  foldr' f z (Idmap _ m) = foldr' f z m
  foldl f z (Idmap _ m) = foldl f z m
  foldl' f z (Idmap _ m) = foldl' f z m
  foldr1 f (Idmap _ m) = foldr1 f m
  foldl1 f (Idmap _ m) = foldl1 f m
  toList (Idmap _ m) = toList m
  null (Idmap _ m) = null m
  length (Idmap _ m) = length m
  elem x (Idmap _ m) = elem x m
  maximum (Idmap _ m) = maximum m
  minimum (Idmap _ m) = minimum m
  sum (Idmap _ m) = sum m
  product (Idmap _ m) = product m

empty :: Idmap a
empty =
  Idmap 0 IntMap.empty

insert :: (Coercible id Int, HasField "id" a a id id) => a -> Idmap a -> Idmap a
insert x (Idmap n xs) =
  Idmap n (IntMap.insert (coerce (view (field @"id") x)) x xs)

-- | Insert a new element while assigning its id.
insertNew ::
     ( Coercible id Int
     , HasField "id" a a id id
     )
  => a
  -> Idmap a
  -> (a, Idmap a)
insertNew x (Idmap n xs) =
  let
    x' =
      x & field @"id" .~ coerce n
  in
    (x', Idmap (n+1) (IntMap.insert n x' xs))

delete :: (Coercible id Int, HasField "id" a a id id) => a -> Idmap a -> Idmap a
delete x (Idmap n xs) =
  Idmap n (IntMap.delete (coerce (view (field @"id") x)) xs)
