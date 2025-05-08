module Core.Types (module Core.Types) where

import Builtin.Keys (ttArrowTyConKey)

import Core.Types.Subst

import Types.Unique (Unique, HasUnique(..), ComparingUniq(..))
import Types.Unique.FM
import Outputable
import Panic

import Data.Maybe (isJust)
import Data.Text qualified as T
import GHC.Stack (HasCallStack)

data Name = Name
  { name_text :: T.Text
  , name_sort :: NameSort
  , name_uniq :: {-# UNPACK #-} !Unique
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

class HasName a where
  getName :: a -> Name

instance HasName Name where
  getName = id

mkInternalName :: T.Text -> Unique -> Name
mkInternalName t u = Name{name_text=t, name_sort=Internal, name_uniq=u}

-- | A thing which can be typechecked.
data TypedThing
  = AnId Id
  | ADataCon DataCon
  | ATyCon   TyCon

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

instance HasName Var where
  getName = var_name

instance Eq Var where
  v1 == v2 = var_uniq v1 == var_uniq v2

instance Ord Var where
  v1 `compare` v2 = var_uniq v1 `compare` var_uniq v2

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
  | DataConWorkId
  | DataConFunId

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
      | (tvs, ty') <- splitForAllTys ty
      = parensIf (p > 6) $
          text "forall" <+> hsep (map ppr tvs) <> dot <+> go 6 ty'

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
  | tyConUniq tc == ttArrowTyConKey = fun_case
  | otherwise = Nothing
  where
    fun_case
      | (t1:t2:rest) <- args
      = assert (null rest) $ Just (t1, t2)
      | otherwise
      = Nothing

-- | Decompose a function type into its argument and return types,
-- panicking if that is not possible.
splitFunTy :: Type -> (Type, Type)
splitFunTy ty = maybe (panic "splitFunTy") id $ splitFunTy_maybe ty

-- | Attempt to decompose a function type into its argument and return types.
splitFunTy_maybe :: Type -> Maybe (Type, Type)
splitFunTy_maybe ty
  | FunTy arg res <- coreFullView ty = Just (arg, res)
  | otherwise                        = Nothing

splitFunTys :: Type -> ([Type], Type)
splitFunTys ty = split [] ty ty
  where
    split args _    (FunTy arg res) = split (arg : args) res res
    split args orig ty1 | Just ty2 <- coreView ty1 = split args orig ty2
    split args orig _               = (reverse args, orig)

splitForAllTys :: Type -> ([TyVar], Type)
splitForAllTys ty = split [] ty
  where
    split tvs (ForAllTy tv ty') = split (tv : tvs) ty'
    split tvs other = (reverse tvs, other)


-- | Try to get the type variable under a 'Type', and panic if it isn't
-- a type variable type.
getTyVar :: HasCallStack => Type -> TyVar
getTyVar ty = maybe (panic "getTyVar") id (getTyVar_maybe ty)

-- | Try to get the type variable under a 'Type'.
getTyVar_maybe :: Type -> Maybe TyVar
getTyVar_maybe = repGetTyVar_maybe . coreFullView

-- | Try to get the type variable under a 'Type', without expanding synonyms.
repGetTyVar_maybe :: Type -> Maybe TyVar
repGetTyVar_maybe (TyVarTy tv) = Just tv
repGetTyVar_maybe _            = Nothing

isTyVarTy :: Type -> Bool
isTyVarTy ty = isJust (getTyVar_maybe ty)

-- coreView: Taken from GHC.
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
coreView :: Type -> Maybe Type
-- ^ This function strips off the /top layer only/ of a type synonym
-- application (if any) its underlying representation type.
-- Returns 'Nothing' if there is nothing to look through.
--
-- This function does not look through type family applications.
--
-- By being non-recursive and inlined, this case analysis gets efficiently
-- joined onto the case analysis that the caller is already doing.
coreView (TyConApp tc tys) = expandSynTyConApp_maybe tc tys
coreView _                 = Nothing
-- See Note [Inlining coreView].
{-# INLINE coreView #-}

coreFullView, core_full_view :: Type -> Type
-- ^ Iterates 'coreView' until there is no more to synonym to expand.
-- NB: coreFullView is non-recursive and can be inlined;
--     core_full_view is the recursive one
-- See Note [Inlining coreView].
coreFullView ty@(TyConApp tc _)
  | isSynonymTyCon tc = core_full_view ty
coreFullView ty = ty
{-# INLINE coreFullView #-}

core_full_view ty
  | Just ty' <- coreView ty = core_full_view ty'
  | otherwise               = ty

-----------------------------------------------
-- | @expandSynTyConApp_maybe tc tys@ expands the RHS of type synonym @tc@
-- instantiated at arguments @tys@, or returns 'Nothing' if @tc@ is not a
-- synonym.
expandSynTyConApp_maybe :: TyCon -> [Type] -> Maybe Type
{-# INLINE expandSynTyConApp_maybe #-}
-- This INLINE will inline the call to expandSynTyConApp_maybe in coreView,
-- which will eliminate the allocation Just/Nothing in the result
-- Don't be tempted to make `expand_syn` (which is NOINLINE) return the
-- Just/Nothing, else you'll increase allocation
expandSynTyConApp_maybe tc arg_tys
  | Just (tvs, rhs) <- synTyConDefn_maybe tc
  , arg_tys `saturates` tyConArity tc
  = Just $! (expand_syn tvs rhs arg_tys)
    -- Why $! ?
    -- Every client of this will evaluate the (expand_syn ...) thunk, so it's
    -- better to just not build it. That said, this function is marked INLINE,
    -- and the client context should be enough for GHC to avoid thunk
    -- construction anyway.
  | otherwise
  = Nothing

saturates :: [Type] -> Arity -> Bool
saturates _       0 = True
saturates []      _ = False
saturates (_:tys) n = assert (n >= 0) $ saturates tys (n-1)
                       -- Arities are always positive; the assertion just checks
                       -- that, to avoid an ininite loop in the bad case

-- | A helper for 'expandSynTyConApp_maybe' to avoid inlining this cold path
-- into call-sites.
--
-- Precondition: the call is saturated or over-saturated;
--               i.e. length tvs <= length arg_tys
expand_syn :: [TyVar]  -- ^ the variables bound by the synonym
           -> Type     -- ^ the RHS of the synonym
           -> [Type]   -- ^ the type arguments the synonym is instantiated at.
           -> Type
{-# NOINLINE expand_syn #-} -- We never want to inline this cold-path.

expand_syn tvs rhs arg_tys
  -- No substitution necessary if either tvs or tys is empty
  -- This is both more efficient, and steers clear of an infinite
  -- loop; see Note [Care using synonyms to compress types]
  | null arg_tys  = assert (null tvs) rhs
  | null tvs      = mkAppTys rhs arg_tys
  | otherwise     = go empty_subst tvs arg_tys
  where
    empty_subst = mkEmptySubst in_scope
    in_scope = mkInScopeSet $ shallowTyVarsOfTypes arg_tys
      -- The free vars of 'rhs' should all be bound by 'tenv',
      -- so we only need the free vars of tys
      -- See also Note [The substitution invariant] in GHC.Core.TyCo.Subst.

    go subst [] tys
      | null tys  = rhs'  -- Exactly Saturated
      | otherwise = mkAppTys rhs' tys
          -- Its important to use mkAppTys, rather than (foldl AppTy),
          -- because the function part might well return a
          -- partially-applied type constructor; indeed, usually will!
      where
        rhs' = substTy subst rhs

    go subst (tv:tvs') (ty:tys) = go (extendTvSubst subst tv ty) tvs' tys

    go _ (_:_) [] = panic "expand_syn" -- (ppr tvs $$ ppr rhs $$ ppr arg_tys)
                   -- Under-saturated, precondition failed

{- Note [Inlining coreView]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
It is very common to have a function

  f :: Type -> ...
  f ty | Just ty' <- coreView ty = f ty'
  f (TyVarTy ...) = ...
  f ...           = ...

If f is not otherwise recursive, the initial call to coreView
causes f to become recursive, which kills the possibility of
inlining. Instead, for non-recursive functions, we prefer to
use coreFullView, which guarantees to unwrap top-level type
synonyms. It can be inlined and is efficient and non-allocating
in its fast path. For this to really be fast, all calls made
on its fast path must also be inlined, linked back to this Note.

Note [Care using synonyms to compress types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Using a synonym to compress a type has a tricky wrinkle. Consider
(in GHC terms) coreView applied to (TyConApp LiftedRep [])

* coreView expands the LiftedRep synonym:
     type LiftedRep = BoxedRep Lifted

* Danger: we might apply the empty substitution to the RHS of the
  synonym.  And substTy calls mkTyConApp BoxedRep [Lifted]. And
  mkTyConApp compresses that back to LiftedRep.  Loop!

* Solution: in expandSynTyConApp_maybe, don't call substTy for nullary
  type synonyms.  That's more efficient anyway.
-}

type Arity = Int
data TyCon =
  TyCon 
    { tyConUniq    :: {-# UNPACK #-} !Unique -- ^ must match unique of name
    , tyConName    :: !Name
    , tyConBinders :: [TyVar]  -- ^ Must all be TyVars.
    , tyConResKind :: !Kind    -- ^ The result kind of the tycon.
                                -- Right now, this can only be 'Star'.
    , tyConKind    :: !Kind    -- ^ Cached kind of the tycon
    , tyConArity   :: !Arity   -- ^ Cached @length tycon_binders@.
    , tyConNullaryApp :: !Type -- ^ Cached TyConApp tycon [].
    , tyConDetails :: !TyConDetails
    }
    deriving (Eq, Ord) via ComparingUniq TyCon

instance Outputable TyCon where
  ppr tc = ppr (tyConName tc)

instance HasUnique TyCon where
  getUnique = tyConUniq

instance HasName TyCon where
  getName = tyConName

mkTyCon :: Name -> [TyVar] -> TyConDetails -> TyCon
mkTyCon name vars details =
  let tc = TyCon
        { tyConUniq    = name_uniq name
        , tyConName    = name
        , tyConBinders = binders
        , tyConResKind = Star
        , tyConKind    = mkTyConKind vars Star
        , tyConArity   = length vars
        , tyConNullaryApp = TyConApp tc []
        , tyConDetails = details
        }
  in tc
  where
    binders = map (\v -> assert (isTyVar v) v) vars

mkPrimTyCon :: Name -> [TyVar] -> TyCon
mkPrimTyCon name binders =
  mkTyCon name binders PrimTyCon

mkAlgDataTyCon :: Name -> [TyVar] -> [DataCon] -> TyCon
mkAlgDataTyCon name binders dcs =
  mkTyCon name binders $ AlgTyCon
    $ DataTyCon {data_cons = dcs, data_cons_size = (length dcs)}

mkTyConTy :: TyCon -> Type
mkTyConTy = tyConNullaryApp

-- | Given the binders and result kind of a tycon, construct the kind.
mkTyConKind :: [TyVar] -> Kind -> Kind
mkTyConKind vars resk = go varKinds resk
  where
    varKinds = map var_kind_ vars
    go [] rk = rk
    go (vk:vks) rk = vk `mkArrowKind` go vks rk

isAlgTyCon, isSynonymTyCon, isPrimTyCon :: TyCon -> Bool
isAlgTyCon (TyCon{tyConDetails=AlgTyCon{}}) = True
isAlgTyCon _                                 = False

isSynonymTyCon (TyCon{tyConDetails=SynonymTyCon{}}) = True
isSynonymTyCon _                                     = False

isPrimTyCon (TyCon{tyConDetails=PrimTyCon{}}) = True
isPrimTyCon _                                  = False

-- | Extract the 'TyVar's bound by a vanilla type synonym and the
-- corresponding (unsubstituted) right hand side.
synTyConDefn_maybe :: TyCon -> Maybe ([TyVar], Type)
synTyConDefn_maybe (TyCon{tyConBinders=tyvars, tyConDetails=details})
  | SynonymTyCon {syntc_rhs = ty} <- details
  = Just (tyvars, ty)
  | otherwise
  = Nothing

-- | Extract the RHS of a type synonym's @type@ declaration.
synTyConRhs_maybe :: TyCon -> Maybe Type
synTyConRhs_maybe (TyCon{tyConDetails = details}) = case details of
  SynonymTyCon {syntc_rhs} -> Just syntc_rhs
  _                        -> Nothing

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
    -- | Right hand side of a type derived from a @data@ declaration.
    -- This includes types with no constructors.
  = DataTyCon
    { data_cons :: [DataCon]
      -- ^ The data type constructors, can be empty.
      -- INVARIANT: Kept in order of increasing tag.
    , data_cons_size :: Int
      -- ^ Cached (length data_cons)
    }
  | TupleTyCon
    { data_con :: DataCon
      -- someday, add tuple_sort :: TupleSort, which is
      -- boxed or unboxed. (Or maybe do this sooner, so we can have
      -- the constraint tuple.)
    }
    -- | TyCon derived from a @newtype@ declaration.
  | NewTyCon
    { data_con :: DataCon
      -- ^ The unique newtype constructor.
    , nt_rhs :: Type
      -- ^ Cached argument type of the constructor (i.e. the repr type).
    }

instance Outputable AlgTyConRhs where
  ppr (DataTyCon{data_cons}) =
    pprDeeperList (hsep . punctuate (space <> vbar)) $ map ppr data_cons
  ppr (TupleTyCon{data_con}) = ppr data_con
  ppr (NewTyCon  {data_con}) = ppr data_con

tyConRhsDataCons :: AlgTyConRhs -> [DataCon]
tyConRhsDataCons (DataTyCon{data_cons}) = data_cons
tyConRhsDataCons (TupleTyCon{data_con}) = [data_con]
tyConRhsDataCons (NewTyCon  {data_con}) = [data_con]

tyConDataCons :: TyCon -> [DataCon]
tyConDataCons tc = maybe [] id (tyConDataCons_maybe tc)

tyConDataCons_maybe :: TyCon -> Maybe [DataCon]
tyConDataCons_maybe (TyCon{tyConDetails = details})
  | AlgTyCon{algtc_rhs = rhs} <- details
  = Just $ tyConRhsDataCons rhs
tyConDataCons_maybe _ = Nothing

{-
Data Constructors
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

'DataCon': All about data constructors.
Note: "Worker functions" are not like GHC's "wrapper functions."
We will also have wrapper functions in the future.

Note [Data Constructor Naming]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Each data constructor C has at least two, and up to three,
Names associated with it:

                           text     namespace  name of?
----------------------------------------------------------
  1. The "data con itself"    C     DataName    DataCon
  2. The datacon worker       C     VarName     Id
  3. The datacon function     $FC   VarName     Id

Every non-nullary data constructor defined by @data@
recieves a datacon function.

It is not necessary to give every data constructor a function,
but I have set up this infrastructure very early in the days of the compiler
and I believe that this will simplify the code generation
(otherwise, a function would need to be generated on-the-fly at some point
and then eliminated by CSE with all the other sites that needed a function.
In the future, this would not be able to cross modules either, which seems
to be a limitation of GHC.)

Each of these three names has a distinct Unique. The output of the source
code renamer will always be the first name, and the typechecker will translate
it to the worker datacon.

Note [Data constructor workers and functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For a @data@-declared type, the worker datacon is the actual data constructor
and has no unfolding.
When the Core2ANF pass encounters a saturated worker datacon, it translates
to the appropriate allocation directly. When it encounters an unsaturated
worker datacon, it is translated to a use of the function.
**This is the only way for a datacon function reference to appear.**

Newtypes don't get constructor functions.
The datacon worker is semantically the identity function.
Unfortunately, the type of the datacon and the type of the identity function
are different, so we cannot inline the datacon,
as our program would become ill-typed. This will be fixed in the future
when we switch to System FC as the core language and utilise coercions.
When the Core2ANF pass encounters the datacon worker,
it is simply removed if possible.
If not possible (consider, e.g. @map MkN [...]@) then it is replaced by 'id'.

Note [The future: constructor wrappers]

As we add more powerful type system features, especially GADT syntax,
and field unpacking, it will become necessary to have a "wrapper" that
translates between the source type of the datacon and the internal
representation type. GHC does this when those two types differ.
We currently have no need for this. The wrapper type and the function
type may very well be different, but the worker type and the function
type will always be the same.
-}

type ConTag = Int

data DataCon =
  DataCon
    { dcName :: Name
    , dcUniq :: Unique -- ^ Cached from Name
    , dcTag  :: ConTag -- ^ This constructor's tag; orders 'DataCon's.

    -- Fields about the type
    -- ~~~~~~~~~~~~~~~~~~~~~

      -- | Universally quantified type vars in the type of this 'DataCon'.
      -- If this is [a,b,c] then the return type of the data constructor
      -- is exactly (T a b c).
      -- This might not be the same as the 'tycon_binders' in the 'TyCon'.
    , dcUnivTyVars :: [TyVar]
      -- | The type of the data constructor.
      -- This is also the type of the function, if it exists.
    , dcType  :: Type   -- a -> T a b
      -- | Result type of the data constructor.
    , dcTycon :: TyCon  -- T a b

      -- | Argument types (decomposed from dc_type).
      -- INVARIANT: Must agree with dc_type!
    , dcArgTys :: [Type]
      -- | Result type (decomposed from dc_type).
      -- INVARIANT: Must agree with dc_type!
    , dcResTy  :: Type
      -- | Cached datacon arity.
    , dcArity  :: Arity  -- INVARIANT: dc_arity == length dc_arg_tys

    -- Fields about the record-ness of this constructor
    -- TODO: Add this when we add record syntax
    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    -- , dc_fields :: [FieldLabel]

    -- Fields about the data constructor worker
    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      -- | The curried worker function that corresponds to the constructor.
      -- When the code generator sees this saturated, it allocates.
    , dcWorkId :: Id

    -- Fields about the data constructor function
    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    , dcFunId :: Maybe Id
    }
    deriving (Eq, Ord) via ComparingUniq DataCon

instance HasName DataCon where getName = dcName
instance HasUnique DataCon where getUnique = dcUniq
instance Outputable DataCon where
  ppr DataCon{dcName, dcTag, dcType} =
    viewPprDebug $ \debug ->
      if debug
        then ppr dcName <> lbrack <> hsep 
          [ppr dcTag <> comma, ppr dcType] <> rbrack
        else ppr dcName

mkDataCon :: Name     -- ^ Name for this DataCon
          -> ConTag   -- ^ Tag for this DataCon
          -> Type     -- ^ This DataCon's type
          -> Id       -- ^ An Id for the datacon worker
          -> Maybe Id -- ^ An Id for the datacon function, if one is wanted
          -> TyCon    -- ^ The TyCon to which this DataCon belongs
          -> DataCon
mkDataCon n tag ty wid fid tc = DataCon
  { dcName = n
  , dcUniq = getUnique n
  , dcTag  = tag
  
  , dcUnivTyVars = qvars
  , dcType       = ty
  , dcArgTys     = argTys
  , dcResTy      = resTy
  , dcArity      = length argTys

  , dcTycon  = tc
  , dcWorkId = wid
  , dcFunId  = fid
  }
  where
    (qvars, tau) = splitForAllTys ty
    (argTys, resTy) = splitFunTys tau

data Literal
  = LitChar Char
  | LitNumber !Integer -- ^ Numeric literal

    -- note in the future that LitNumber will get much more interesting as it
    -- becomes possible to write polymorphic numeric literals.
    -- I'm not certain how we'll want to represent Integer, when we add that,
    -- but probably as the construction of the appropriate Integer data value
    -- rather than as a Literal.

  | LitString !T.Text -- ^ String literal; we can emit constant strings today
                      -- but have no idea how they should be manipulated.
  | LitRubbish  -- ^ A nonsense value. Its type is @forall a. a@.
  deriving (Eq, Ord)

instance Outputable Literal where
  ppr = pprLit

pprLit :: Literal -> Doc
pprLit (LitChar c)   = quotes $ char c
pprLit (LitNumber i) = ppr i
pprLit (LitString t) = doubleQuotes $ text t
pprLit (LitRubbish)  = text "RUBBISH"

{-
Note [Constructor tag allocation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When typechecking, we need to allocate constructor tags to constructors.
They are allocated based on the order of constructors in the data_cons field
of TyCon, with the first constructor getting fIRST_TAG.

-}
mkTyConTagMap :: TyCon -> UniqFM Name ConTag
mkTyConTagMap tycon =
  listToUFM $ map getName (tyConDataCons tycon) `zip` [fIRST_TAG ..]

-- Start tags at 1. Someday these will be actual tags on a pointer,
-- and you can't tag a pointer with zero.
fIRST_TAG :: ConTag
fIRST_TAG = 1
