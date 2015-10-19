{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternGuards   #-}
---------------------------------------------------
--  ____  _ __  __                ___                  _ _____
-- | __ )(_)  \/  | __ _ _ __    / _ \ _   _  __ _  __| |_   _| __ ___  ___
-- |  _ \| | |\/| |/ _` | '_ \  | | | | | | |/ _` |/ _` | | || '__/ _ \/ _ \
-- | |_) | | |  | | (_| | |_) | | |_| | |_| | (_| | (_| | | || | |  __/  __/
-- |____/|_|_|  |_|\__,_| .__/   \__\_\\__,_|\__,_|\__,_| |_||_|  \___|\___|
--                      |_|
--
-- Module:  Data.BiMap
-- Author:  sternenseemann, Bez
-- Licence: LGPL-3
---------------------------------------------------

module Data.BiMap.QuadTree
  ( empty
  , fromList
  , toList
  , insert
  , lookup
  , (!)
    ) where
   

import           Prelude        hiding (lookup)

import           Data.Bifunctor
import           Data.Maybe     (Maybe (..))

-- | Operators
infixl 9 !

-- | A bidirectional Map
data BiMap k y = Branch
  { key        :: k
  , yek        :: y
  , smallSmall :: BiMap k y
  , smallGreat :: BiMap k y
  , greatSmall :: BiMap k y
  , greatGreat :: BiMap k y
  }
  | Leaf
  deriving (Show, Eq)

-- | This is no real bimapping, since the bimap might not be correct anyomore at all
-- it is recommended to use bimap' , for since it has certainty for the bimap to be in
-- correct order afterwards
instance Bifunctor BiMap where
  bimap _  _  Leaf = Leaf 
  bimap fk fy (Branch key yek ss sg gs gg) = Branch (fk key) (fy yek)
    (bimap fk fy ss) (bimap fk fy sg) (bimap fk fy gs) (bimap fk fy gg)

instance Functor (BiMap a) where
  fmap _ Leaf = Leaf
  fmap f (Branch key yek ss sg gs gg) = Branch key (f yek)
    (fmap f ss) (fmap f sg) (fmap f gs) (fmap f gg)

-- | Really bimapping over the bimap and applying the functions
bimap' :: (Ord a, Ord b, Ord c, Ord d) => (a -> b) -> (c -> d) -> BiMap a c -> BiMap b d
bimap' fk fy map = let l = toList map 
  in fromList (fmap (bimap fk fy) l) 

-- | Really bimapping over the bimap
realbimap :: (Ord a, Ord b, Ord c, Ord d) => (a -> b) -> (c -> d) -> BiMap a c -> BiMap b d
realbimap = bimap'

-- | The empty BiMap
empty :: BiMap k y
empty = Leaf

-- | A BiMap containing only the relation between one element of type k and one element of type y
singleton :: (Ord k, Ord y) => k -> y -> BiMap k y
singleton key yek = insert key yek empty

-- | Inserts a relation between a key and a yek into the BiMap
insert :: (Ord k, Ord y) => k -> y -> BiMap k y -> BiMap k y
insert key yek Leaf = Branch key yek Leaf Leaf Leaf Leaf
insert key yek (Branch key' yek' ss sg gs gg)
  | key < key' && yek < yek' = Branch key' yek' (insert key yek ss) sg gs gg
  | key < key' && yek > yek' = Branch key' yek' ss (insert key yek sg) gs gg
  | key > key' && yek < yek' = Branch key' yek' ss sg (insert key yek gs) gg
  | key > key' && yek > yek' = Branch key' yek' ss sg gs (insert key yek gg)
  | otherwise                = error "key and yek must be unique"

-- | Constructs a BiMap from a List of tuples consisting of a key and yek
fromList :: (Ord k, Ord y) => [(k, y)] -> BiMap k y
fromList = foldl (flip . uncurry $ insert) empty

-- | Giving back a BiMap as a List of tuples
toList :: (Ord k, Ord y) => BiMap k y -> [(k, y)]
toList (Branch k y ss sg gs gg) = (k, y): toList ss ++ toList sg ++ toList gs ++ toList gg ++ []
toList Leaf = []

-- | Infix alias to lookup
(!) :: (Ord k, Ord y) => BiMap k y -> Either k y -> Maybe (k, y)
bimap ! query = lookup query bimap

-- | Looks up Either a (Left key) or a (Right yek) in a BiMap 
lookup :: (Ord k, Ord y) => Either k y -> BiMap k y -> Maybe (k, y)
lookup _ Leaf = Nothing
lookup query@(Left key) (Branch key' yek' ss sg gs gg)
  | key == key' = Just (key', yek')
  | key <  key' = doubleLookup query ss sg
  | key >  key' = doubleLookup query gg gs
lookup query@(Right yek) (Branch key' yek' ss sg gs gg)
  | yek == yek' = Just (key', yek')
  | yek <  yek' = doubleLookup query ss gs
  | yek >  yek' = doubleLookup query gg sg

-- | Internal recursion function for lookup
doubleLookup :: (Ord k, Ord y) => Either k y -> BiMap k y -> BiMap k y -> Maybe (k, y)
doubleLookup query a b = pickJust (lookup query a) (lookup query b)

-- | Takes two Maybes and returns if one of these are a Just, Nothing if both are Nothing or Just
-- Used internally in lookup
pickJust :: Maybe a -> Maybe a -> Maybe a
pickJust Nothing Nothing   = Nothing
pickJust (Just _) (Just _) = Nothing
pickJust (Just x) Nothing  = Just x
pickJust Nothing (Just y)  = Just y





-- | Test BiMap
humanization :: BiMap String Int
humanization = fromList [ ("two", 2)
                        , ("three", 3)
                        , ("nine", 9)
                        , ("ten", 10)
                        , ("four", 4)
                        , ("five", 5)
                        , ("eight", 8)
                        , ("seven", 7)
                        , ("six", 6)
                        , ("eleven", 11)
                        , ("twelve", 12)
                        , ("one", 1)
                        , ("zero", 0) ]

