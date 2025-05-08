module Types.Unique.Set
  ( UniqSet, getUniqSet',
    emptyUniqSet,
    unitUniqSet,
    mkUniqSet,
    mkUniqSetDirectly,
    addOneToUniqSet,
    addListToUniqSet,
    delFromUniqSet, delListFromUniqSet, delFromUniqSet_Directly,
    elementOfUniqSet, lookupUniqSet, lookupUniqSet_Directly,
    filterUniqSet, mapUniqSet, partitionUniqSet,
    plusUniqSet,
    unionManyUniqSets,
    minusUniqSet,
    intersectUniqSets,
    sizeUniqSet,
    isNullUniqSet,
    nonDetFoldUniqSet, nonDetEltsUniqSet,
    allUniqSet,
    anyUniqSet
  ) where

import Types.Unique.FM
import Types.Unique 
import Outputable

newtype UniqSet a = UniqSet { getUniqSet' :: UniqFM a a }

-- Construction

emptyUniqSet :: UniqSet a
emptyUniqSet = UniqSet emptyUFM

unitUniqSet :: HasUnique a => a -> UniqSet a
unitUniqSet x = UniqSet (unitUFM x x)

mkUniqSet :: HasUnique a => [a] -> UniqSet a
mkUniqSet xs = UniqSet (listToUFM [(x, x) | x <- xs])

mkUniqSetDirectly :: HasUnique a => [a] -> UniqSet a
mkUniqSetDirectly xs = UniqSet (listToUFM [(x, x) | x <- xs])

-- Insertion

addOneToUniqSet :: HasUnique a => UniqSet a -> a -> UniqSet a
addOneToUniqSet (UniqSet m) x = UniqSet (addToUFM m x x)

addListToUniqSet :: HasUnique a => UniqSet a -> [a] -> UniqSet a
addListToUniqSet = foldl addOneToUniqSet

-- Deletion

delFromUniqSet :: HasUnique a => UniqSet a -> a -> UniqSet a
delFromUniqSet (UniqSet m) x = UniqSet (delFromUFM m x)

delListFromUniqSet :: HasUnique a => UniqSet a -> [a] -> UniqSet a
delListFromUniqSet (UniqSet m) xs = UniqSet $ delListFromUFM m xs

delFromUniqSet_Directly :: UniqSet a -> Unique -> UniqSet a
delFromUniqSet_Directly (UniqSet m) u = UniqSet $ delFromUFM_Directly m u

-- Query

elementOfUniqSet :: HasUnique a => a -> UniqSet a -> Bool
elementOfUniqSet x (UniqSet m) = elemUFM x m

lookupUniqSet :: HasUnique a => UniqSet a -> a -> Maybe a
lookupUniqSet (UniqSet m) = lookupUFM m

lookupUniqSet_Directly :: UniqSet a -> Unique -> Maybe a
lookupUniqSet_Directly (UniqSet m) = lookupUFM_Directly m

sizeUniqSet :: UniqSet a -> Int
sizeUniqSet (UniqSet m) = sizeUFM m

isNullUniqSet :: UniqSet a -> Bool
isNullUniqSet (UniqSet m) = isNullUFM m

nonDetEltsUniqSet :: UniqSet a -> [a]
nonDetEltsUniqSet (UniqSet m) = nonDetEltsUFM m

-- Set operations

plusUniqSet :: UniqSet a -> UniqSet a -> UniqSet a
plusUniqSet (UniqSet m1) (UniqSet m2) = UniqSet (plusUFM m1 m2)

unionManyUniqSets :: [UniqSet a] -> UniqSet a
unionManyUniqSets = foldl plusUniqSet emptyUniqSet

minusUniqSet :: UniqSet a -> UniqSet a -> UniqSet a
minusUniqSet (UniqSet m1) (UniqSet m2) = UniqSet $ minusUFM m1 m2

intersectUniqSets :: UniqSet a -> UniqSet a -> UniqSet a
intersectUniqSets (UniqSet m1) (UniqSet m2) = UniqSet $ intersectUFM m1 m2

-- Functional ops

filterUniqSet :: (a -> Bool) -> UniqSet a -> UniqSet a
filterUniqSet p (UniqSet m) = UniqSet (filterUFM p m)

-- This is pretty terrible...
mapUniqSet :: HasUnique b => (a -> b) -> UniqSet a -> UniqSet b
mapUniqSet f = mkUniqSet . map f . nonDetEltsUniqSet

partitionUniqSet :: (a -> Bool) -> UniqSet a -> (UniqSet a, UniqSet a)
partitionUniqSet f (UniqSet m) = (UniqSet l, UniqSet r)
  where (l, r) = partitionUFM f m

-- | Fold over a unique set. This fold is nondeterministic in the sense of
-- unique nondeterminism, unless the function is commutative.
nonDetFoldUniqSet :: (a -> b -> b) -> b -> UniqSet a -> b
nonDetFoldUniqSet f z (UniqSet m) = nonDetFoldUFM f z m

allUniqSet :: (a -> Bool) -> UniqSet a -> Bool
allUniqSet p (UniqSet m) = allUFM p m

anyUniqSet :: (a -> Bool) -> UniqSet a -> Bool
anyUniqSet p (UniqSet m) = anyUFM p m

-- Instances

instance Semigroup (UniqSet a) where
  (<>) = plusUniqSet

instance Monoid (UniqSet a) where
    mempty = emptyUniqSet
    mappend = (<>)

-- Output-ery

instance Outputable a => Outputable (UniqSet a) where
    ppr us = ppr $ getUniqSet' us
