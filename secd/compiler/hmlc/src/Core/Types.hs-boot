module Core.Types (module Core.Types) where

data Var
data Type
data TyCon
data TyConDetails
data DataCon

type Id    = Var
type TyVar = Var

mkAppTy :: Type -> Type -> Type
mkTyConApp :: TyCon -> [Type] -> Type
getTyVar_maybe :: Type -> Maybe TyVar
