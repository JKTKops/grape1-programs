module Types.Var.Set
  ( -- * Sets of Var, Id, and TyVar
    VarSet, IdSet, TyVarSet
    -- * Manipulating these sets
  , emptyVarSet, unitVarSet, mkVarSet, extendVarSet, extendVarSetList
  , elemVarSet, subVarSet, unionVarSet, unionVarSets, mapUnionVarSet
  , intersectVarSet, intersectsVarSet, disjointVarSet, isEmptyVarSet
  , delVarSet, delVarSetList, delVarSetByKey, minusVarSet, filterVarSet
  , anyVarSet, allVarSet
  , transCloVarSet, fixVarSet, lookupVarSet, lookupVarSet_Directly
  , lookupVarSetByName
  , sizeVarSet, elemVarSetByKey
  , partitionVarSet
  ) where

import Core.Types (Var, Id, TyVar, Name)
import Types.Unique (Unique, getUnique)
import Types.Unique.Set
import Types.Unique.FM (elemUFM_Directly)

type VarSet   = UniqSet Var
type IdSet    = UniqSet Id
type TyVarSet = UniqSet TyVar

emptyVarSet :: VarSet
emptyVarSet = emptyUniqSet

unitVarSet :: Var -> VarSet
unitVarSet  = unitUniqSet

mkVarSet :: [Var] -> VarSet
mkVarSet = mkUniqSet

extendVarSet :: VarSet -> Var -> VarSet
extendVarSet = addOneToUniqSet

extendVarSetList :: VarSet -> [Var] -> VarSet
extendVarSetList = addListToUniqSet

elemVarSet :: Var -> VarSet -> Bool
elemVarSet = elementOfUniqSet

elemVarSetByKey :: Unique -> VarSet -> Bool
elemVarSetByKey u vs = elemUFM_Directly u (getUniqSet' vs)

subVarSet :: VarSet -> VarSet -> Bool
subVarSet vs1 vs2 = isEmptyVarSet $ vs1 `minusVarSet` vs2

unionVarSet :: VarSet -> VarSet -> VarSet
unionVarSet = plusUniqSet

unionVarSets :: [VarSet] -> VarSet
unionVarSets = unionManyUniqSets

mapUnionVarSet :: (a -> VarSet) -> [a] -> VarSet
mapUnionVarSet asSet = foldr (unionVarSet . asSet) emptyVarSet

intersectVarSet :: VarSet -> VarSet -> VarSet
intersectVarSet = intersectUniqSets

intersectsVarSet :: VarSet -> VarSet -> Bool
intersectsVarSet vs1 vs2 = not $ disjointVarSet vs1 vs2

disjointVarSet :: VarSet -> VarSet -> Bool
disjointVarSet vs1 vs2 = isEmptyVarSet $ intersectVarSet vs1 vs2

isEmptyVarSet :: VarSet -> Bool
isEmptyVarSet vs = sizeVarSet vs == 0

delVarSet :: VarSet -> Var -> VarSet
delVarSet = delFromUniqSet

delVarSetList :: VarSet -> [Var] -> VarSet
delVarSetList = delListFromUniqSet

delVarSetByKey :: VarSet -> Unique -> VarSet
delVarSetByKey = delFromUniqSet_Directly

minusVarSet :: VarSet -> VarSet -> VarSet
minusVarSet = minusUniqSet

filterVarSet :: (Var -> Bool) -> VarSet -> VarSet
filterVarSet = filterUniqSet

anyVarSet, allVarSet :: (Var -> Bool) -> VarSet -> Bool
anyVarSet = anyUniqSet
allVarSet = allUniqSet

-- | @transCloVarSet f vs@ computes the transitive closure of vs under f.
transCloVarSet :: (VarSet -> VarSet) -> VarSet -> VarSet
-- Repeatedly apply f to new candidates until reaching a fixed point.
-- We compute the vars that are new and add only those each time, so it's
-- critical that f {v1,v2} === f v1 `union` f v2. If f needs to see the whole
-- set at once, use fixVarSet instead.
transCloVarSet f seeds = go seeds seeds where
  go :: VarSet {- acc -} -> VarSet {- worklist -} -> VarSet
  go acc candidates
    | isEmptyVarSet newVs = acc
    | otherwise           = go (acc `unionVarSet` newVs) newVs
    where newVs = f candidates `minusVarSet` acc

-- | Repeatedly apply f until the set reaches a fixed point.
-- Unlike 'transCloVarSet', f will see the whole set, every time.
fixVarSet :: (VarSet -> VarSet) -> VarSet -> VarSet
fixVarSet f vars
  | newVs `subVarSet` vars = vars
  | otherwise              = fixVarSet f newVs
  where newVs = f vars

lookupVarSet :: VarSet -> Var -> Maybe Var
lookupVarSet = lookupUniqSet

lookupVarSet_Directly :: VarSet -> Unique -> Maybe Var
lookupVarSet_Directly = lookupUniqSet_Directly

lookupVarSetByName :: VarSet -> Name -> Maybe Var
lookupVarSetByName vs = lookupVarSet_Directly vs . getUnique

sizeVarSet :: VarSet -> Int
sizeVarSet = sizeUniqSet

partitionVarSet :: (Var -> Bool) -> VarSet -> (VarSet, VarSet)
partitionVarSet = partitionUniqSet
