module Core.Builtin (module Core.Builtin) where

import Data.Text qualified as T

import Builtin.Keys
import Builtin.Prim
import Types.Unique
import Types.Unique.FM -- replace with NameEnv when we have it
import Core.Types
import Panic (panic)

-------------------------------------------------------------------
---- wired-in and builtin things.
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

mkBuiltinDataConName :: T.Text -> Unique -> DataCon -> Name
mkBuiltinDataConName n u dc =
  Name { name_uniq = u, name_text = n
       , name_sort = Builtin (ADataCon dc)}

mkPrimDataCon :: Name     -- ^ Name for this DataCon
              -> Type     -- ^ This DataCon's type
              -> TyCon    -- ^ The TyCon to which this DataCon belongs
              -> DataCon
mkPrimDataCon name ty tycon = dataCon
  where
    tagMap = mkTyConTagMap tycon
    tag = maybe (panic "mkPrimDataCon") id (lookupUFM tagMap name)

    dataCon = mkDataCon name tag ty workId Nothing tycon
      -- lookupNameEnv_NF

    workKey = unsafeIncPrimKey $ getUnique name
    workName = Name { name_uniq = workKey, name_text = name_text name
                    , name_sort = Builtin (AnId (dcWorkId dataCon))}
    workId = Id
      { var_name = workName, var_uniq = workKey, var_type_ = ty
      , id_keepalive_ = KeepAlive, id_details_ = DataConWorkId
      , id_info_ = IdInfo
      }
      -- Types.Id.Make.mkDataConWorkId workName dataCon

----------------------------------------------
-- wired in types

{- Note [Int Boxity]
~~~~~~~~~~~~~~~~~~~~
Right now, all of the primitive types (Int, Bool, Char, and String)
are implicitly boxed. Strings in particular are a bit magical and we haven't
actually settled on any particular way of manipulating them in the interpreter
itself, so certainly haven't settled on anything here. (5/3/25)

This implicit boxing is because the interpreter has no notion of an unboxed
primitive value, and the interpreter's support for untyped languages and GC
is not amenable to unboxed values.

Note [Boxity: The Future]
~~~~~~~~~~~~~~~~~~~~~~~~~
In the future, we will follow the ideas of Types are Calling Conventions,
and we will have a kind like
  typedata RuntimeRep = BoxedRep | IntRep | CharRep | ...
  typedata TYPE (rr : RuntimeRep)
  kind Type = TYPE BoxedRep
then we will have
  data Int : Type = I# Int#
and also have the (builtin)
  data Int# : TYPE IntRep

This is appealing **even for an eagerly evaluated language** because there is
no guarantee that the representation BoxedRep things is the same as the
representation of IntRep things on real hardware -- for example, existing ETCa
simulations today can have 32-bit pointers (BoxedRep) but 64-bit integers
(IntRep). Aggressive specialization, W/W transformations, and inlining will
be key to performance, and this is no different as for any other polymorphic
language.

When compiling to bytecode, however, we are not able to manipulate values whose
types have kinds other than BoxedRep. The SECD interpreter does not (currently)
have any way to manipulate such values. It is possible to unbox and rebox an
primitive object, but if you are so unfortunate as to trigger a GC with the
reboxing, the unboxed value on the stack will be treated as a pointer and the
interpreter crashes. This is no good.

We will need a compulsory pass as part of lowering ANF to bytecode, which boxes
all values whose representations are otherwise unboxed. This will unfortunately
nuke some W/W transformations, but this is ultimately not a costly miss.
We could even try to undo them if we wanted, though I don't advise this.
Once every variable once again has a boxed representation, generating bytecode
is easy.

Be aware when performing this pass, it will probably be easiest to leave the
primops like +# in their +# form. Part of "reboxing everything" means modifying
functions to assume that their arguments are boxed, and so we will be able
to view the type +#: Int# -> Int# -> Int# as if it were Int -> Int -> Int while
generating code, and this is exactly the type of the ADD bytecode.
-}

intTyName :: Name
intTyName = mkBuiltinTyConName "Int" intTyConKey intTyCon
intTyCon :: TyCon
intTyCon = mkPrimTyCon intTyName []
intTy :: Type
intTy = mkTyConTy intTyCon

boolTyName :: Name
boolTyName = mkBuiltinTyConName "Bool" boolTyConKey boolTyCon
boolTyCon :: TyCon
boolTyCon = mkAlgDataTyCon boolTyName [] [falseDataCon, trueDataCon]
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

-- wired in data constructors
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~

falseDataConName :: Name
falseDataConName = mkBuiltinDataConName "False" falseDataConKey falseDataCon
falseDataCon :: DataCon
falseDataCon = mkPrimDataCon falseDataConName boolTy boolTyCon

trueDataConName :: Name
trueDataConName = mkBuiltinDataConName "True" trueDataConKey trueDataCon
trueDataCon :: DataCon
trueDataCon = mkPrimDataCon trueDataConName boolTy boolTyCon
