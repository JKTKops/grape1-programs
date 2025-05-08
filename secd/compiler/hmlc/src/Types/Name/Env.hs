module Types.Name.Env
  ( -- * Maps keyed by Name
    NameEnv
    -- * Manipulating these maps
  , emptyNameEnv, unitNameEnv, mkNameEnv, extendNameEnv, extendNameEnvList
  , elemNameEnv, subNameEnv, unionNameEnv, unionNameEnvs, mapUnionNameEnv
  , intersectNameEnv, intersectsNameEnv, disjointNameEnv, isEmptyNameEnv
  , delNameEnv, delNameEnvList, delNameEnvByKey, minusNameEnv, filterNameEnv
  , anyNameEnv, allNameEnv
  , transCloNameEnv, fixNameEnv, lookupNameEnv, lookupNameEnv_Directly
  , lookupNameEnvByName
  , sizeNameEnv, elemNameEnvByKey
  , partitionNameEnv
  ) where

import Core.Types   (Name)
import Types.Unique (Unique, getUnique)
import Types.Unique.FM

type NameEnv   = UniqFM Name

emptyNameEnv :: NameEnv a
emptyNameEnv = emptyUFM

unitNameEnv :: Name -> a -> NameEnv a
unitNameEnv  = unitUFM

mkNameEnv :: [(Name, a)] -> NameEnv a
mkNameEnv = listToUFM

extendNameEnv :: NameEnv a -> Name -> a -> NameEnv a
extendNameEnv = addToUFM

extendNameEnvList :: NameEnv a -> [(Name, a)] -> NameEnv a
extendNameEnvList = addListToUFM

elemNameEnv :: Name -> NameEnv a -> Bool
elemNameEnv = elemUFM

elemNameEnvByKey :: Unique -> NameEnv a -> Bool
elemNameEnvByKey = elemUFM_Directly

subNameEnv :: NameEnv a -> NameEnv a -> Bool
subNameEnv vs1 vs2 = isEmptyNameEnv $ vs1 `minusNameEnv` vs2

unionNameEnv :: NameEnv a -> NameEnv a -> NameEnv a
unionNameEnv = plusUFM

unionNameEnvs :: [NameEnv a] -> NameEnv a
unionNameEnvs = plusUFMList

mapUnionNameEnv :: (a -> NameEnv b) -> [a] -> NameEnv b
mapUnionNameEnv asSet = foldr (unionNameEnv . asSet) emptyNameEnv

intersectNameEnv :: NameEnv a -> NameEnv a -> NameEnv a
intersectNameEnv = intersectUFM

intersectsNameEnv :: NameEnv a -> NameEnv a -> Bool
intersectsNameEnv vs1 vs2 = not $ disjointNameEnv vs1 vs2

disjointNameEnv :: NameEnv a -> NameEnv a -> Bool
disjointNameEnv vs1 vs2 = isEmptyNameEnv $ intersectNameEnv vs1 vs2

isEmptyNameEnv :: NameEnv a -> Bool
isEmptyNameEnv vs = sizeNameEnv vs == 0

delNameEnv :: NameEnv a -> Name -> NameEnv a
delNameEnv = delFromUFM

delNameEnvList :: NameEnv a -> [Name] -> NameEnv a
delNameEnvList = delListFromUFM

delNameEnvByKey :: NameEnv a -> Unique -> NameEnv a
delNameEnvByKey = delFromUFM_Directly

minusNameEnv :: NameEnv a -> NameEnv a -> NameEnv a
minusNameEnv = minusUFM

filterNameEnv :: (a -> Bool) -> NameEnv a -> NameEnv a
filterNameEnv = filterUFM

anyNameEnv, allNameEnv :: (a -> Bool) -> NameEnv a -> Bool
anyNameEnv = anyUFM
allNameEnv = allUFM

-- | @transCloNameEnv f vs@ computes the transitive closure of vs under f.
transCloNameEnv :: forall a. (NameEnv a -> NameEnv a) -> NameEnv a -> NameEnv a
-- Repeatedly apply f to new candidates until reaching a fixed point.
-- We compute the vars that are new and add only those each time, so it's
-- critical that f {v1,v2} === f v1 `union` f v2. If f needs to see the whole
-- set at once, use fixNameEnv instead.
transCloNameEnv f seeds = go seeds seeds where
  go :: NameEnv a {- acc -} -> NameEnv a {- worklist -} -> NameEnv a
  go acc candidates
    | isEmptyNameEnv newVs = acc
    | otherwise           = go (acc `unionNameEnv` newVs) newVs
    where newVs = f candidates `minusNameEnv` acc

-- | Repeatedly apply f until the set reaches a fixed point.
-- Unlike 'transCloNameEnv', f will see the whole set, every time.
fixNameEnv :: (NameEnv a -> NameEnv a) -> NameEnv a -> NameEnv a
fixNameEnv f vars
  | newVs `subNameEnv` vars = vars
  | otherwise              = fixNameEnv f newVs
  where newVs = f vars

lookupNameEnv :: NameEnv a -> Name -> Maybe a
lookupNameEnv = lookupUFM

lookupNameEnv_Directly :: NameEnv a -> Unique -> Maybe a
lookupNameEnv_Directly = lookupUFM_Directly

lookupNameEnvByName :: NameEnv a -> Name -> Maybe a
lookupNameEnvByName vs = lookupNameEnv_Directly vs . getUnique

sizeNameEnv :: NameEnv a -> Int
sizeNameEnv = sizeUFM

partitionNameEnv :: (a -> Bool) -> NameEnv a -> (NameEnv a, NameEnv a)
partitionNameEnv = partitionUFM
