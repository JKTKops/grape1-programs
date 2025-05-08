-- Substitution into types.
module Core.Types.Subst (module Core.Types.Subst) where

import {-# SOURCE #-} Core.Types (mkAppTy, mkTyConApp, getTyVar_maybe)

-- these will stay warnings until it's implemented
-- shallowTyVarsOfTypes belongs somewhere else, and will require
-- VarSets, so we're a bit away from that still.
-- Regardless, this isn't very important, because it won't cause
-- anything to crash until we try to write a type synonym.
mkEmptySubst = undefined
mkInScopeSet = undefined
shallowTyVarsOfTypes = undefined
substTy = undefined
extendTvSubst = undefined
