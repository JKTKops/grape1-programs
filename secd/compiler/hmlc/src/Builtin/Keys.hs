-- | All of the Builtin 'Unique's in the compiler.

{- (?) Note [Known-key names]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Obviously, it is crucial that the compiler assigns the correct
known Name to anything that is wired-in.
This is handled primarily in two places:

  1. In the parser, when parsing builtin syntax, we can immediately
     drop-in the known-key name for the thing we parsed. (e.g.
     when we parse [] in a type, we can just drop in listTyCon directly.)

  2. In the renamer, whose initial environment
     is initialized with these keys. If you add a known-key name,
     it is critical that that name be added to the appropriate list
     for initializing the renamer!

Here, we define the keys for the known names. Known names may
be defined variously (see, e.g., Types.Builtin) and it is crucial
that they are added to the appropriate list in Builtin.Names.
-}
module Builtin.Keys (module Builtin.Keys) where

import Types.Unique

{- Note [Wired-In Keys in the Future]
Something which is "Wired In" is either a builtin, or is something
defined in the prelude code which the compiler knows about.

In the future, with support for modules, we will have to include
more information about these things in their names, such as which
module has its source code. The compiler will need this so that it
can make sure the wired-in definition gets the correct unique.
Most of this could happen in Builtin.Names, because most names that
are known-key are just WiredIn, and not actually Builtin.

For now, the Prelude code is stapled on front of the source code,
anything that wasn't used is thrown away before optimizing,
and compilation otherwise proceeds as you'd expect. Therefore we can
assume that the only definition for a name that's been wired-in
is the definition we're looking for. If there is more than definition,
then the user has tried to redefine a builtin thing, which (for now)
is simply not allowed.

Each Key is a Unique, and should have an associated Name in
Builtin.Names (possibly imported from somewhere else).
-}

-- Building blocks

mkAlphaTyVarKey :: Int -> Unique
mkAlphaTyVarKey = mkUniqueInt '0'

mkPreludeTyConKey :: Int -> Unique
mkPreludeTyConKey = mkUniqueInt '2'

mkPreludeDataConKey :: Int -> Unique
mkPreludeDataConKey i = mkUniqueInt '3' (2*i)

{- Note [Keys for Many Things]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Some primitive keys are actually keys for multiple things which need
separate keys. For example, the key for a dataconstructor is actually two
keys: one for the datacon itself, and another for its worker.
Therefore, some mk...Key functions allocate many keys per input value.

To get from the allocated key to its friend(s), use unsafeIncPrimKey
and unsafeStepPrimKey.
-}

-- | Increment the value of a 'Unique'. See Note [Keys for Many Things].
-- DO NOT use this to generate 'Unique's on-the-fly in generic compiler code.
unsafeIncPrimKey :: Unique -> Unique
unsafeIncPrimKey = unsafeReconstructUnique . (+1) . getKey

-- | Step the value of a 'Unique' by the given number.
-- See Note [Keys for Many Things].
unsafeStepPrimKey :: Int -> Unique -> Unique
unsafeStepPrimKey n = unsafeReconstructUnique . (+n) . getKey

-----------------------------------------------
-- Known TyCon keys

intTyConKey, boolTyConKey, charTyConKey, stringTyConKey,
   ctArrowTyConKey, ttArrowTyConKey, ccArrowTyConKey :: Unique

intTyConKey          = mkPreludeTyConKey 0
boolTyConKey         = mkPreludeTyConKey 1
charTyConKey         = mkPreludeTyConKey 2
stringTyConKey       = mkPreludeTyConKey 3

ttArrowTyConKey      = mkPreludeTyConKey 4 -- ->
ctArrowTyConKey      = mkPreludeTyConKey 5 -- =>
ccArrowTyConKey      = mkPreludeTyConKey 6 -- ==>


------------------------------------------------
-- Known DataCon keys
-- ~~~~~~~~~~~~~~~~~~

falseDataConKey, trueDataConKey :: Unique
falseDataConKey = mkPreludeDataConKey 0
trueDataConKey  = mkPreludeDataConKey 1

