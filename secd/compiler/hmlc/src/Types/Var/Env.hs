module Types.Var.Env
  ( -- * Maps keyed by Var, Id, and TyVar
    VarEnv, IdEnv, TyVarEnv
    -- * Manipulating these maps
  , emptyVarEnv, unitVarEnv, mkVarEnv, extendVarEnv, extendVarEnvList
  , elemVarEnv, subVarEnv, unionVarEnv, unionVarEnvs, mapUnionVarEnv
  , intersectVarEnv, intersectsVarEnv, disjointVarEnv, isEmptyVarEnv
  , delVarEnv, delVarEnvList, delVarEnvByKey, minusVarEnv, filterVarEnv
  , anyVarEnv, allVarEnv
  , transCloVarEnv, fixVarEnv, lookupVarEnv, lookupVarEnv_Directly
  , lookupVarEnvByName
  , sizeVarEnv, elemVarEnvByKey
  , partitionVarEnv
  ) where

import Core.Types (Var, Id, TyVar, Name)
import Types.Unique (Unique, getUnique)
import Types.Unique.FM

type VarEnv   = UniqFM Var
type IdEnv    = UniqFM Id
type TyVarEnv = UniqFM TyVar

emptyVarEnv :: VarEnv a
emptyVarEnv = emptyUFM

unitVarEnv :: Var -> a -> VarEnv a
unitVarEnv  = unitUFM

mkVarEnv :: [(Var, a)] -> VarEnv a
mkVarEnv = listToUFM

extendVarEnv :: VarEnv a -> Var -> a -> VarEnv a
extendVarEnv = addToUFM

extendVarEnvList :: VarEnv a -> [(Var, a)] -> VarEnv a
extendVarEnvList = addListToUFM

elemVarEnv :: Var -> VarEnv a -> Bool
elemVarEnv = elemUFM

elemVarEnvByKey :: Unique -> VarEnv a -> Bool
elemVarEnvByKey = elemUFM_Directly

subVarEnv :: VarEnv a -> VarEnv a -> Bool
subVarEnv vs1 vs2 = isEmptyVarEnv $ vs1 `minusVarEnv` vs2

unionVarEnv :: VarEnv a -> VarEnv a -> VarEnv a
unionVarEnv = plusUFM

unionVarEnvs :: [VarEnv a] -> VarEnv a
unionVarEnvs = plusUFMList

mapUnionVarEnv :: (a -> VarEnv b) -> [a] -> VarEnv b
mapUnionVarEnv asSet = foldr (unionVarEnv . asSet) emptyVarEnv

intersectVarEnv :: VarEnv a -> VarEnv a -> VarEnv a
intersectVarEnv = intersectUFM

intersectsVarEnv :: VarEnv a -> VarEnv a -> Bool
intersectsVarEnv vs1 vs2 = not $ disjointVarEnv vs1 vs2

disjointVarEnv :: VarEnv a -> VarEnv a -> Bool
disjointVarEnv vs1 vs2 = isEmptyVarEnv $ intersectVarEnv vs1 vs2

isEmptyVarEnv :: VarEnv a -> Bool
isEmptyVarEnv vs = sizeVarEnv vs == 0

delVarEnv :: VarEnv a -> Var -> VarEnv a
delVarEnv = delFromUFM

delVarEnvList :: VarEnv a -> [Var] -> VarEnv a
delVarEnvList = delListFromUFM

delVarEnvByKey :: VarEnv a -> Unique -> VarEnv a
delVarEnvByKey = delFromUFM_Directly

minusVarEnv :: VarEnv a -> VarEnv a -> VarEnv a
minusVarEnv = minusUFM

filterVarEnv :: (a -> Bool) -> VarEnv a -> VarEnv a
filterVarEnv = filterUFM

anyVarEnv, allVarEnv :: (a -> Bool) -> VarEnv a -> Bool
anyVarEnv = anyUFM
allVarEnv = allUFM

-- | @transCloVarEnv f vs@ computes the transitive closure of vs under f.
transCloVarEnv :: forall a. (VarEnv a -> VarEnv a) -> VarEnv a -> VarEnv a
-- Repeatedly apply f to new candidates until reaching a fixed point.
-- We compute the vars that are new and add only those each time, so it's
-- critical that f {v1,v2} === f v1 `union` f v2. If f needs to see the whole
-- set at once, use fixVarEnv instead.
transCloVarEnv f seeds = go seeds seeds where
  go :: VarEnv a {- acc -} -> VarEnv a {- worklist -} -> VarEnv a
  go acc candidates
    | isEmptyVarEnv newVs = acc
    | otherwise           = go (acc `unionVarEnv` newVs) newVs
    where newVs = f candidates `minusVarEnv` acc

-- | Repeatedly apply f until the set reaches a fixed point.
-- Unlike 'transCloVarEnv', f will see the whole set, every time.
fixVarEnv :: (VarEnv a -> VarEnv a) -> VarEnv a -> VarEnv a
fixVarEnv f vars
  | newVs `subVarEnv` vars = vars
  | otherwise              = fixVarEnv f newVs
  where newVs = f vars

lookupVarEnv :: VarEnv a -> Var -> Maybe a
lookupVarEnv = lookupUFM

lookupVarEnv_Directly :: VarEnv a -> Unique -> Maybe a
lookupVarEnv_Directly = lookupUFM_Directly

lookupVarEnvByName :: VarEnv a -> Name -> Maybe a
lookupVarEnvByName vs = lookupVarEnv_Directly vs . getUnique

sizeVarEnv :: VarEnv a -> Int
sizeVarEnv = sizeUFM

partitionVarEnv :: (a -> Bool) -> VarEnv a -> (VarEnv a, VarEnv a)
partitionVarEnv = partitionUFM
