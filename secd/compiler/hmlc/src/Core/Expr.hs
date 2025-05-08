module Core.Expr (module Core.Expr) where

import Core.Types
import Control.Exception (assert)

import Data.Int (Int32)
import Data.Text qualified as T

infixl 4 `mkApps`, `mkTyApps`, `mkVarApps`, `App`

{-|
The core internal expression type.

Currently, this is a System F lambda calculus with pattern matching.

If you modify this type, you probably need to modify the HML formalism
in Core.Lint.
-}
data Expr b
  = Var Id
  | Lit Literal
  | App (Expr b) (Arg b)
  | Lam b (Expr b)
  | Let (Bind b) (Expr b)
  | Case (Expr b) b Type [Alt b]
  | Type Type

-- | An expression in the argument position of an App.
-- INVARIANT: This is the only place that 'Expr.Type' can appear.
type Arg = Expr

data Alt b = Alt AltCon [b] (Expr b)

data Bind b
  = NonRec b (Expr b)
  | Rec [(b, Expr b)]

data AltCon
  = DataAlt DataCon
  | LitAlt Literal
    -- ^ A literal, e.g. @case e of { 1 -> ... }@. See Note [LitAlt boxed?]
  | DEFAULT -- ^ Catch-all alternative: @case e of { _ -> ... }@

instance Ord AltCon where
  a1 `compare` a2 = go a1 a2 where
    {-# INLINE go #-}
    go DEFAULT       DEFAULT       = EQ
    go DEFAULT       _             = LT
    go (DataAlt dc1) (DataAlt dc2) = dcTag dc1 `compare` dcTag dc2
    go (DataAlt{})   _             = LT
    go (LitAlt l1)   (LitAlt l2)   = l1 `compare` l2
    go _             _             = GT

instance Eq AltCon where
  ac1 == ac2 = ac1 `compare` ac2 == EQ

{- Note [LitAlt lifted?]
~~~~~~~~~~~~~~~~~~~~~~~~
We have no notion of lifted or unlifted literals right now, because the backend
for version one is an interpreter with only boxed values.
In future native-code backends, we very well will want an unboxed values,
but we DO NOT want @Int@ to be an unboxed type. See Note [Int Boxity]
in Core.Builtin, and in particular Note [Boxity: The Future] in the same.

When we have unboxed types, 'LitAlt' probably needs to be limited to only
match on unboxed literals. How will this integrate with the future of boxity?
We will continue to optimize in the middle end as normal, but when compiling
for the bytecode backend, there will be a compulsory pass in the ANF stage
which reboxes everything so that unboxed values are never manipulated.
Generating bytecode for the boxed types is straightforward.

-------------------------------- Core INVARIANTS ------------------------------

Note [Expr.Type occurrences]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The Expr.Type constructor may /only/ appear in places where the type is marked
as 'Arg b', not 'Expr b'. This is exactly the argument position of an
application.

This is used to represent type applications to polymorphic functions,
and corresponds to selecting the type for a @forall@ at the head of
a polymorphic function's type.

Note [Shadowing in Core]
~~~~~~~~~~~~~~~~~~~~~~~~
It seems natural to assume that there is no shadowing in any Core expressions.
After all, we are attaching 'Unique's to everything!

The simplifier does not GUARANTEE that shadowing is avoided. Any pass that you
write must work fine even in the presence of arbitrary shadowing.

This does indeed mean that the 'Unique's on 'Var's are not really unique.
But it is very useful to get a constant time comparison on Vars, and to get
good keys for maps and sets. If you do want to eliminate shadowing in the AST,
you can give a new 'Unique' to each (Internal) Id without changing its name,
which helps with debugging.

It would be easier to have a no-shadowing invariant, and we do our best in the
simplifier to clone variables that are shadowed. But it is extremely difficult
to GUARANTEE that there is no shadowing when doing, for example, CSE.

Since we cannot easily guarantee it anyway, we fully commit to the bit.
Types.Id.mkTemplateLocal will (when we have it) make up local binders with
locally-unique 'Unique's, but they will not guarantee global uniqueness.
This is the tradeoff for not having to thread a 'UniqueSupply' to this
and similar functions.

Note [Case Expression Invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Case expressions are the most complex element of the core language (and the
most difficult to compile). As such, they come with a number of invariants,
all of which would ideally be checked by Core.Lint:

1. The list of case alternatives may be empty, but...

2. The list of alternatives must be exhaustive. An /exhaustive/ case does not
   necessarily mention all constructors:
   @
      data Foo = Red | Green | Blue
      ... case x of
            Red   -> True
            other -> f (case x of
                          Green -> ...
                          Blue  -> ... ) ...
   @

   The inner case does not need a @Red@ alternative, because @x@ cannot be
   @Red@ at that point. This is not checked by Core.Lint because it is very
   difficult to do. Imagine if the inner case were floated out. It would be
   extremely difficult to tell that the case is exhaustive, but it is.

   We nonetheless assume throughout that case expressions are exhaustive.
   If you have a case expression that is /not/ exhaustive, we may generate
   segfaults. For example, imagine the constructors of Blue had fields.
   For the case above, we may generate code that tests for Green. If the test
   failed, we may simply /assume/ that x is Blue, and start trying to fetch
   the fields of a Blue constructor, leading to a horrible death.

3. The DEFAULT case alternative is first, if it is present.

4. The remaining cases are in order of strictly increasing
      tag, if they are 'DataAlt's or
      lit, if they are 'LitAlt's
   This makes it much easier to find the relevant constructor.

5. The 'ty' field of (Case scrut bndr ty alts) is the type of the /entire/
   case expression. Why is this needed? After all, we could easily gather
   the type from the type of the first arm. But see invariant #1 above!
   Not only might caching it here be faster, it may also be necessary in
   the presence of GADTs, when the type of the RHS may not be equal to the
   type of the overall case expression (unless you have the local context
   of that arm).

6. Obviously, the type of the scrutinee must match the type of the binder.

Note [Empty case alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The alternatives of a case expression can be empty only if the case alts are
unreachable. This would usually be true because the scrutinee will diverge.
For example, given @data Void@, there is no expression that could fill in for
@e@ such that @case e of Void -> Any _ { }@ is reachable. There are such @e@
which make this expression well-typed, however.

Note [Join Points]
~~~~~~~~~~~~~~~~~~
We do not currently do any form of join point optimization, but it is planned
for the future. See Note [Join points] in GHC.Core and the paper
  Luke Maurer, Paul Downen, Zena Ariola, and Simon Peyton Jones. "Compiling
  without continuations." Submitted to PLDI'17.
  https://www.microsoft.com/en-us/research/publication/compiling-without-continuations/

-}

---------------------------------------------------------------------
-- Useful synonyms
---------------------------------------------------------------------
 
 {-
Note [CoreProgram]
~~~~~~~~~~~~~~~~~~
The top-level bindings of a program, a CoreProgram, are a list of CoreBinds.

Later bindings in the list can refer to earlier ones, but not vice versa.
So this is OK:
    NonRec { x = 4 }
    Rec { p = ...q...x...
        ; q = ...p...x  }
    Rec { f = ...p...x...f }
    NonRec { g = ...f...q...x... }
But it would NOT be okay for f to refer to g.

The occurrence analyser does strongly-connected component analysis on each
Rec binding and splits it into a sequence of smaller bindings if possible.
So the program typically starts life as a single giant Rec and then breaks
apart into smaller chunks.
-}

type CoreProgram = [CoreBind] -- see Note [CoreProgram]
type CoreExpr    = Expr CoreBinder
type CoreAlt     = Alt  CoreBinder
type CoreBind    = Bind CoreBinder
type CoreArg     = Arg  CoreBinder
type CoreBinder  = Var

---------------------------------------------------------------------
-- Constructing CoreExprs
---------------------------------------------------------------------

-- | Apply a list of argument expressions to a function expression in nested
-- fashion. Prefer 'Core.Make.mkCoreApps' if possible.
mkApps    :: Expr b -> [Arg b] -> Expr b
-- | Apply a list of type argument expressions to a function expression in
-- nested fashion.
mkTyApps  :: Expr b -> [Type]  -> Expr b
-- | Apply a list of type or value variables to a function expression
-- in nested fashion.
mkVarApps :: Expr b -> [Var]   -> Expr b
-- | Apply a list of argument expressions to a data constructor in nested
-- fashion. Prefer 'Core.Make.mkCoreConApps' if possible.
mkConApp  :: DataCon -> [Arg b] -> Expr b

mkApps f args     = foldl' App f args
mkTyApps f args   = foldl' (\e a -> e `App` mkTyArg a) f args
mkVarApps f vars  = foldl' (\e a -> e `App` varToCoreExpr a) f vars
mkConApp con args = mkApps (Var (dcWorkId con)) args

mkTyArg :: Type -> Arg b
mkTyArg ty = Type ty

-- | Create a machine literal expression of type @Int@ from an 'Integer'.
-- You are responsible for checking that the number is in-bounds.
-- See Note [Boxity: The Future]:
--   someday this will make a literal of type @Int#@.
-- Therefore, you should prefer Core.Make.mkIntExpr for future-proofing.
mkIntLit :: Integer -> Expr b
mkIntLit n = Lit $ LitNumber n

-- | Create a machine integer literal of type @Int@ from an 'Integer',
-- wrapping if necessary.
-- Someday, this will make a literal of type @Int#@. Therefore,
-- prefer Core.Make.mkIntExpr.
mkIntLitWrap :: Integer -> Expr b
mkIntLitWrap n = Lit $ LitNumber (fromIntegral @Int32 (fromIntegral n))

-- same caveat as mkInt
-- | Prefer Core.Make.mkCharExpr.
mkCharLit :: Char -> Expr b
-- | Prefer Core.Make.mkStringExpr
mkStringLit :: String -> Expr b

mkCharLit   c = Lit (LitChar c)
mkStringLit s = Lit (LitString $ T.pack s)

-- | Bind all supplied binding groups over an expression in a nested let-expr.
-- Prefer to use 'Core.Make.mkCoreLets' if possible,
-- which may check invariants.
mkLets :: [Bind b] -> Expr b -> Expr b
-- | Bind all supplied binders over an expression in a nested lambda.
-- Prefer 'Core.Make.mkCoreLams' if possible, which may check invariants.
mkLams :: [b] -> Expr b -> Expr b

mkLams binders body = foldr Lam body binders
mkLets binds body   = foldr mkLet body binds

mkLet :: Bind b -> Expr b -> Expr b
-- never generate empty recursive groups, in fact we will make sure that
-- Core.Lint rejects this. Get rid of them immediately.
mkLet (Rec []) body = body 
mkLet bind     body = Let bind body

mkLetNonRec :: b -> Expr b -> Expr b -> Expr b
mkLetNonRec b rhs body = Let (NonRec b rhs) body

mkLetRec :: [(b, Expr b)] -> Expr b -> Expr b
mkLetRec [] body = body
mkLetRec bs body = Let (Rec bs) body

varToCoreExpr :: CoreBinder -> Expr b
varToCoreExpr v
  | isTyVar v = Type (mkTyVarTy v)
  | otherwise = assert (isId v) $ Var v

varsToCoreExprs :: [CoreBinder] -> [Expr b]
varsToCoreExprs vs = map varToCoreExpr vs

---------------------------------------------------------------------
-- Simple Access
---------------------------------------------------------------------


