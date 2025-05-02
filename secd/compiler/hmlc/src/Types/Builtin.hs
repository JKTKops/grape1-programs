module Types.Builtin (module Types.Builtin) where

import Data.Text qualified as T

import Builtin.Keys
import Builtin.Prim
import Types.Unique
import Core.Types

-------------------------------------------------------------------
---- Names for wired-in Types.
-------------------------------------------------------------------

{- Note [Builtin Things]
If something is fully builtin, it needs not only a key, but its complete
internal representation to be defined here. This is currently the case
for the basic data types and for lists.

If it is wired-in but not builtin, then only its key need be here,
and its definition must appear in the prelude code.
-}

mkBuiltinTyConName :: T.Text -> Unique -> TyCon -> Name
mkBuiltinTyConName n u tc =
  Name { name_uniq = u, name_text = n
       , name_sort = Builtin (ATyCon tc)
       }

----------------------------------------------
-- wired in types (keys 0 -- 50)

intTyName :: Name
intTyName = mkBuiltinTyConName "Int" intTyConKey intTyCon
intTyCon :: TyCon
intTyCon = mkPrimTyCon intTyName []
intTy :: Type
intTy = mkTyConTy intTyCon

boolTyName :: Name
boolTyName = mkBuiltinTyConName "Bool" boolTyConKey boolTyCon
boolTyCon :: TyCon
boolTyCon = mkPrimTyCon boolTyName []
boolTy :: Type
boolTy = mkTyConTy boolTyCon

charTyName :: Name
charTyName = mkBuiltinTyConName "Char" charTyConKey charTyCon
charTyCon :: TyCon
charTyCon = mkPrimTyCon charTyName []
charTy :: Type
charTy = mkTyConTy charTyCon

stringTyName :: Name
stringTyName = mkBuiltinTyConName "String" stringTyConKey stringTyCon
stringTyCon :: TyCon
stringTyCon = mkPrimTyCon stringTyName []
stringTy :: Type
stringTy = mkTyConTy stringTyCon

funTyConName :: Name
funTyConName = mkBuiltinTyConName "->" ttArrowTyConKey funTyCon
funTyCon :: TyCon
funTyCon = mkPrimTyCon funTyConName [alphaTyVar, betaTyVar]
funTy :: Type
funTy = mkTyConTy funTyCon
