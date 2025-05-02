module Core.Types where

import Types.Unique (Unique, HasUnique(..))
import Outputable
import Panic

import Data.Text qualified as T
import Builtin.Keys (ttArrowTyConKey)

data Name = Name {
    name_text :: T.Text,
    name_sort :: NameSort,
    name_uniq :: {-# UNPACK #-} !Unique
  }

instance Outputable Name where
  ppr Name{name_text, name_uniq, name_sort} = ppr name_text <> ppr_uniq
    where
      ppr_uniq
        | System <- name_sort = uniq_doc
        | otherwise           = mempty
      uniq_doc = text "_" <> ppr name_uniq

instance HasUnique Name where
  getUnique = name_uniq

data NameSort
  = Internal -- internal to the code being compiled
  | System   -- generated on-the-fly by the compiler
  | WiredIn  -- something whose name is known-key but which is defined in code.
    -- someday, WiredIn will hopefully be replaced by External
    -- and those things should have compatible meanings.
  | Builtin TypedThing -- something built directly into the compiler

instance Outputable NameSort where
  ppr Internal  = text "internal"
  ppr System    = text "system"
  ppr WiredIn   = text "wired-in"
  ppr Builtin{} = text "builtin"

mkInternalName :: T.Text -> Unique -> Name
mkInternalName t u = Name{name_text=t, name_sort=Internal, name_uniq=u}

-- | A thing which can be typechecked.
data TypedThing
  = AnId Id
  | ATyCon TyCon

---------------- synonyms

type TyVar   = Var
type TcTyVar = Var -- TyVar for use during typechecking only, might be metavar
type TypeVar = Var -- Definitely a type variable (TyVar or TcTyVar)

type Id = Var -- term-level identifier (isId :: Var -> Bool)

{- Note [Details vs Info]
If something claims to be 'Details', then it is required annotating
information about an object. The thing cannot exist without those
details, and we are simply packing a bunch of them away into a separate
record.

In contrast, if something claims to be 'Info', then it is optional
annotating information about an object. Info is generally useful, and it might
even be the case that an appropriate value must exist, but we might not
know what it is. Therefore, Info often contains Maybe-types.
When Info is present, it is always correct, and if it is absent,
it **must** be possible to make a conservative assumption.
-}

data Var
  = TyVar
    { var_name  :: Name
    , var_uniq  :: {-# UNPACK #-} !Unique
    , var_kind_ :: Kind
    }
  | TcTyVar -- We won't be doing type inference for a while so w/e
    { var_name  :: Name
    , var_uniq  :: {-# UNPACK #-} !Unique
    , var_kind_ :: Kind
--    , tc_tv_details :: TcTyVarDetails
    }
  | Id
    { var_name  :: Name
    , var_uniq  :: {-# UNPACK #-} !Unique
    , var_type_ :: Type
    , id_keepalive_ :: KeepAlive
    , id_details_   :: IdDetails
    , id_info_      :: IdInfo -- unstable, optimizer will modify as it analyses
    }

isTyVar, isTypeVar, isId :: Var -> Bool
isTyVar TyVar{} = True
isTyVar _ = False

isTypeVar TyVar{}   = True
isTypeVar TcTyVar{} = True
isTypeVar _         = False

isId Id{} = True
isId _    = False

-- | Get the 'Kind' of a TyVar or TcTyVar
varKind_maybe :: TypeVar -> Maybe Kind
varKind_maybe TyVar{var_kind_}   = Just var_kind_
varKind_maybe TcTyVar{var_kind_} = Just var_kind_
varKind_maybe Id{}               = Nothing

varType_maybe :: Id -> Maybe Type
varType_maybe Id{var_type_} = Just var_type_
varType_maybe _             = Nothing

pprVarTypeOrKind :: Var -> Doc
pprVarTypeOrKind TyVar{var_kind_}   = ppr var_kind_
pprVarTypeOrKind TcTyVar{var_kind_} = ppr var_kind_
pprVarTypeOrKind Id{var_type_}      = ppr var_type_

instance Outputable Var where
  ppr var = viewPprDebug $ \debug ->
    let ppr_var = viewPprStyle $ \sty -> case var of
          (TyVar {})
            | debug -> brackets (text "tv")
          (TcTyVar {})
            | isPprDump sty || debug -> brackets mempty
          (Id {id_keepalive_})
            | debug -> brackets (ppr id_keepalive_)
          _ -> mempty
    in if debug
        then parens (ppr (var_name var) <+> ppr_var
                    <+> colon <+> pprVarTypeOrKind var)
        else ppr (var_name var) <> ppr_var

instance HasUnique Var where
  getUnique = var_uniq

mkTyVar :: Name -> Kind -> TyVar
mkTyVar n k = TyVar{var_name=n, var_uniq=name_uniq n, var_kind_=k}

data KeepAlive -- ^ Can we discard this thing?
  = KeepAlive   -- ^ Do not let this thing get discarded.
  | NoKeepAlive -- ^ This thing can be discarded.
  deriving (Eq, Show)

instance Outputable KeepAlive where
  ppr ka = viewPprDebug $ \debug ->
    if debug
      then case ka of
        KeepAlive   -> text "keep"
        NoKeepAlive -> emptyDoc
      else emptyDoc

-- This type will surely grow.
data IdDetails
  = VanillaId

-- | Identifier information
--
-- An 'IdInfo' contains optional information about a (term-level) 'Id'.
-- This information can be analysis results or other optimization
-- or code generation guidance. If present, it is always correct.
--
-- Two 'Id's may have different 'IdInfo' even if they have the same 'Unique'
-- because the simplifier updates one but not the other.
-- Generally, info is about a definition, not usage, but there are some
-- exceptions (TODO: specify as they arise).
data IdInfo
  = IdInfo {
    -- nothing yet
  }

-- | Empty 'IdInfo' with no useful information whatsoever.
vanillaIdInfo :: IdInfo
vanillaIdInfo = IdInfo { }


{- Note [Separating Types and Kinds]
GHC unifies the type and kind language, and this simplifies
a small number of very advanced type things (notably, if I understand
correctly, it simplifies the promotion of GADT constructors to types).

I don't plan to support features like that, and the type/kind languages
can always be unified in the future if that changes. For now, we keep
them separate. This also makes it easier to work on the type system
iteratively.
-}

data Kind
  = Star
  | KArrow Kind Kind

instance Outputable Kind where
  ppr = go False where
    go _          Star = text "*"
    go wrapArrows (KArrow k1 k2)
      = parensIf wrapArrows $ go True k1 <+> arrow <+> go False k2

typeKind :: Kind
typeKind = Star

mkArrowKind :: Kind -> Kind -> Kind
mkArrowKind = KArrow
infixr 6 `mkArrowKind`

data Type
  = TyVarTy  TyVar
  | TyConTy  TyCon
  | TyConApp TyCon [Type]
    -- ^ Application of a 'TyCon' (including newtypes and synonyms).
    -- Invariant: a saturated application of 'funTyCon' must use
    -- 'FunTy'. Unsaturated 'FunTyCon's must be 'TyConApp's.
    --
    -- Parameters:
    -- 1) Type constructor being applied.
    -- 2) Type arguments. Might not be enough to saturate the constructor.
    --    Even a type synonym doesn't have to be fully saturated!
    --    Type synonyms can appear unsaturated on the RHS of another synonym.
  | AppTy    Type  Type
    -- ^ Type application to something other than a 'TyCon'.
    -- The first type must therefore be another `AppTy` or a `TyVarTy`.
  | FunTy    {- FunTyFlag -} Type  Type
  | ForAllTy TyVar Type

-- This is definitely a temporary hack instance. The true instance should
-- care rather a lot about why things are being printed -- are we dumping?
-- Is this going to the user? Are there symbolic TyCon names that need to
-- be printed infix? Vice versa?
-- GHC dedicates an entire module to the prettyprinting of types. TODO.
instance Outputable Type where
  ppr = go 0 where
    go :: Int -> Type -> Doc
    go _ (TyVarTy tv) = ppr tv
    go _ (TyConTy tc) = ppr tc
    go p (TyConApp tc tys)
      = parensIf (p > 10) $ hsep (ppr tc : map (go 11) tys)
    go p (AppTy t1 t2)
      = parensIf (p > 10) $ go 10 t1 <+> go 11 t2
    go p (FunTy t1 t2)
      = parensIf (p > 8)  $ go 9 t1 <+> arrow <+> go 8 t2
    go p ty@(ForAllTy {})
      -- Gather up the "spine" of foralls and print them all together.
      -- Really, we should make some effort to rename bound variables to keep
      -- them pretty, but I'm not going to bother for now.
      | (tvs, ty') <- splitForAllTy ty
      = parensIf (p > 6) $
          text "forall" <+> hsep (map ppr tvs) <> dot <+> go 6 ty'

splitForAllTy :: Type -> ([TyVar], Type)
splitForAllTy (ForAllTy tv ty) =
  let (tvs, ty') = splitForAllTy ty
  in (tv:tvs, ty')
splitForAllTy other = ([], other)

mkTyVarTy :: TyVar -> Type
mkTyVarTy tv = assert (isTyVar tv) $ TyVarTy tv

mkTyVarTys :: [TyVar] -> [Type]
mkTyVarTys = map mkTyVarTy

mkFunTy :: Type -> Type -> Type
mkFunTy = FunTy
infixr 6 `mkFunTy`

mkNestedFunTy :: [Type] -> Type -> Type
mkNestedFunTy []         rty = rty
mkNestedFunTy (aty:rest) rty = FunTy aty (mkNestedFunTy rest rty)

mkTyConApp :: TyCon -> [Type] -> Type
mkTyConApp tc [] = mkTyConTy tc
mkTyConApp tc tys
  | Just funTy <- tyConAppFunTy_maybe tc tys
  = funTy
  | otherwise
  -- no need to check arity of tc against tys, because it would be
  -- certainly ill-kinded to apply too many arguments.
  = TyConApp tc tys

mkAppTy :: Type -> Type -> Type
mkAppTy (TyConApp tc tys) t
  = mkTyConApp tc (tys ++ [t])
mkAppTy t1 t2 = AppTy t1 t2
infixr 4 `mkAppTy`

mkAppTys :: Type -> [Type] -> Type
mkAppTys ty1                []   = ty1
mkAppTys (TyConApp tc tys1) tys2 = mkTyConApp tc (tys1 ++ tys2)
mkAppTys ty1                tys2 = foldl' AppTy ty1 tys2

mkForAllTy :: TyVar -> Type -> Type
mkForAllTy = ForAllTy

mkNestedForAllTy :: [TyVar] -> Type -> Type
mkNestedForAllTy vars ty = foldr mkForAllTy ty vars

tyConAppFunTy_maybe :: TyCon -> [Type] -> Maybe Type
tyConAppFunTy_maybe tc tys
  | Just (arg, res) <- tyConAppFun_maybe tc tys
  = Just $ FunTy arg res
  | otherwise = Nothing

tyConAppFun_maybe :: TyCon -> [a] -> Maybe (a, a)
tyConAppFun_maybe tc args
  | tycon_uniq tc == ttArrowTyConKey = fun_case
  | otherwise = Nothing
  where
    fun_case
      | (t1:t2:rest) <- args
      = assert (null rest) $ Just (t1, t2)
      | otherwise
      = Nothing

data TyCon =
  TyCon 
    { tycon_uniq    :: {-# UNPACK #-} !Unique -- ^ must match unique of name
    , tycon_name    :: !Name
    , tycon_binders :: [TyVar] -- ^ Must all be TyVars.
    , tycon_reskind :: !Kind -- ^ The result kind of the tycon.
                             -- Right now, this can only be 'Star'.
    , tycon_kind    :: !Kind -- ^ Cached kind of the tycon
    , tycon_arity   :: !Int  -- ^ Cached @length tycon_binders@.
    , tycon_nullaryApp :: !Type -- ^ Cached TyConApp tycon [].
    , tycon_details :: !TyConDetails
    }

instance Outputable TyCon where
  ppr tc = ppr (tycon_name tc)

instance HasUnique TyCon where
  getUnique = tycon_uniq

mkTyCon :: Name -> [TyVar] -> TyConDetails -> TyCon
mkTyCon name vars details =
  let tc = TyCon
        { tycon_uniq    = name_uniq name
        , tycon_name    = name
        , tycon_binders = binders
        , tycon_reskind = Star
        , tycon_kind    = mkTyConKind vars Star
        , tycon_arity   = length vars
        , tycon_nullaryApp = TyConApp tc []
        , tycon_details = details
        }
  in tc
  where
    binders = map (\v -> assert (isTyVar v) v) vars

mkPrimTyCon :: Name -> [TyVar] -> TyCon
mkPrimTyCon name binders =
  mkTyCon name binders PrimTyCon

mkTyConTy :: TyCon -> Type
mkTyConTy = tycon_nullaryApp

-- | Given the binders and result kind of a tycon, construct the kind.
mkTyConKind :: [TyVar] -> Kind -> Kind
mkTyConKind vars resk = go varKinds resk
  where
    varKinds = map var_kind_ vars
    go [] rk = rk
    go (vk:vks) rk = vk `mkArrowKind` go vks rk

data TyConDetails
  -- | Algebraic data type constructors, defined by
  --    - @data@ declarations
  --    - @newtype@ declarations
  --   and probably more in the future.
  -- Data/newtype/type families will be elsewhere.
  = AlgTyCon
    { algtc_rhs :: AlgTyConRhs
    }
  | SynonymTyCon
    { syntc_rhs :: Type
    }
  -- | This thing is magical and can't be defined in the source language.
  | PrimTyCon


data AlgTyConRhs
