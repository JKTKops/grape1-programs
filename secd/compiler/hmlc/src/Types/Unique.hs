{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Types.Unique 
  ( Unique
  , mkBuiltinUnique, mkUniqueInt, pprUnique

    -- * Things that have Uniques
  , HasUnique(..), hasKey

    -- * UniqueSupplies
  , UniqSupply, mkSplitUniqSupply
  , splitUniqSupply, listSplitUniqSupply, extractUniqFromSupply
  , takeUniqFromSupply, uniqsFromSupply

    -- * Monad and class
  , UniqSM, runUSM
  , getUniqueUs, getUniques
  , MonadUnique(..)
  ) where

import Data.Bits
import Data.Char
import Data.Text qualified as T
import Data.Unique qualified as U
import GHC.Exts (Int(I#), Char(C#), indexCharOffAddr#)

import Outputable
import GHC.IO (unsafeInterleaveIO)
import Control.Monad.Fix (MonadFix(..))

-- Data.Unique uses Integer internally, but the hashUnique function
-- is defined essentially thus (after inlining):
-- hashUnique uniq = case coerce uniq of
--   IS i# -> I# i#
--   IP a# -> word2Int# (indexWordArray# a# 0#)
--   IN a# -> negateInt# $ word2Int# (indexWordArray# a# 0#)
-- In other words, if the Integer is within the range of Int,
-- hashUnique returns that Int.
-- Otherwise, it returns the value truncated (bitwise) into the Int range,
-- applying 2's complement to negative Integers.
--
-- Our compiler will never, ever generate so many uniques that they overflow
-- the Int type. It's a 64 bit integer type. GHC's uniques are just
-- Ints, generated in a much faster way than Data.Unique.
-- So it'll obviously do well for us here.

-- | A value that is globally unique across a whole invocation of the compiler.
newtype Unique = Unique Int
  -- derive Ord so that we can make sets of HasUniq things.
  deriving stock (Eq, Ord)
  deriving newtype Show

-- | Build a 'Unique' from a tag character and a number.
-- We use the tag characters to get a loose sense of where this 'Unique'
-- comes from. See Note [Unique tags].
mkUnique :: Char -> Int -> Unique
mkUnique c i = Unique $ ord c `shiftL` uNIQ_BITS .|. i

-- | Make a 'Unique' directly from a tag and a number.
-- This is exported only for use by modules that define wired-in identifers --
-- if you need to make a unique, you should be using a 'UniqSupply'
-- or your local monad's 'MonadUnique' instance.
mkUniqueInt :: Char -> Int -> Unique
mkUniqueInt = mkUnique

{- Note [Unique tags]
Every 'Unique' has a tag in its top bits that gives us a loose sense of where
the unique comes from. Unsurprisingly, the vast majority of uniques come from
renaming and typechecking.

These tags are in use:
  'r'   renaming
  't'   typechecking
  'B'   builtin things

  '0'   mkAlphaTyVarKey (see Types.Builtin.Prim)
  '1'   reserved for mkPreludeClassKey
  '2'   mkPreludeTyConKey
  '3'   mkPreludeDataConKey
-}

-- | Make a 'Unique' for a builtin thing from a number.
-- You must ensure that the numbers used for different things are different!
mkBuiltinUnique :: Int -> Unique
mkBuiltinUnique = mkUnique 'B'

-- | The number of t
tAG_BITS :: Int
tAG_BITS = 8

uNIQ_BITS :: Int
uNIQ_BITS = 56

uNIQ_MASK :: Int
uNIQ_MASK = (1 `shiftL` uNIQ_BITS) - 1

unpackUnique :: Unique -> (Char, Int)
unpackUnique (Unique u) = (tag, i) where
  tag = chr $ u `shiftR` uNIQ_BITS
  i   = u .&. uNIQ_MASK

class HasUnique a where
  getUnique :: a -> Unique
hasKey :: HasUnique a => a -> Unique -> Bool
x `hasKey` k = getUnique x == k

-- | Unique Supply
--
-- A 'UniqSupply' is unique and supplies exactly /one/ distinct 'Unique'.
-- From the supply, one can manufacture an arbitrary number of further
-- 'UniqueSupply' values, which are distinct from the original and from all
-- others.
--
-- When you force a 'UniqSupply', you also generate the 'Unique' that it
-- supplies. If you don't force it, the 'Unique' is not generated.
-- Try not to force 'UniqSupply's unnecessarily. Obviously, splitting a supply
-- forces it.
--
-- This is taken (modified a bit) from GHC, as is much of this module.
data UniqSupply 
  = MkSplitUniqSupply {-# UNPACK #-} !Unique UniqSupply UniqSupply

-- | Create a 'UniqueSupply' out of thin air. The character given must
-- be distinct from those of all calls to this function in the compiler
-- for the values generated to be truly unique.
--
-- This function is taken from GHC, whose develoeprs have
-- spent years carefully honing it.
mkSplitUniqSupply :: Char -> IO UniqSupply
mkSplitUniqSupply c =
  let mkSupply = unsafeInterleaveIO $ do
        u  <- U.hashUnique <$> U.newUnique
        s1 <- mkSupply
        s2 <- mkSupply
        return $ MkSplitUniqSupply (mkUnique c u) s1 s2
  in mkSupply

-- | Get two 'UniqSupply' from one, each of which supplies its own 'Unique'.
-- If possible, also use the 'Unique' from this supply for something.
splitUniqSupply :: UniqSupply -> (UniqSupply, UniqSupply)
-- | Generate an infinite list of 'UniqSupply' from the given one.
listSplitUniqSupply :: UniqSupply -> [UniqSupply]
-- | Extract the 'Unique' from this 'UniqSupply'
extractUniqFromSupply :: UniqSupply -> Unique
-- | Generate an infinite list of 'Unique' from the given 'UniqSupply'
-- by splitting forever.
uniqsFromSupply :: UniqSupply -> [Unique]
-- | Get a 'Unique' and a new supply from this supply.
takeUniqFromSupply :: UniqSupply -> (Unique, UniqSupply)

splitUniqSupply (MkSplitUniqSupply _ s1 s2) = (s1, s2)
listSplitUniqSupply (MkSplitUniqSupply _ s1 s2) = s1 : listSplitUniqSupply s2
extractUniqFromSupply (MkSplitUniqSupply u _ _) = u
uniqsFromSupply (MkSplitUniqSupply u _ s2) = u : uniqsFromSupply s2
takeUniqFromSupply (MkSplitUniqSupply u s1 _) = (u, s1)

-------------------------------------------------------------------
-- A monad for Unique generation.
-------------------------------------------------------------------

-- We want to mark all of the class functions as INLINE, but we can't
-- put meaningful INLINE pragmas on the names (>>=) etc. since these
-- are just record selectors and don't have meaningful unfoldings.
-- They aren't the functions we really want to inline!
-- So we have to define all of the functions separately and INLINE those.

newtype UniqSM a = USM { unUSM :: UniqSupply -> (# a, UniqSupply #) }

runUSM :: UniqSupply -> UniqSM a -> (a, UniqSupply)
runUSM us (USM f) = case f us of (# a, us' #) -> (a, us')

instance Monad UniqSM where
  (>>=) = bindUSM
  
instance Applicative UniqSM where
  pure = pureUSM
  (<*>) = apUSM

instance Functor UniqSM where
  fmap = fmapUSM

bindUSM :: UniqSM a -> (a -> UniqSM b) -> UniqSM b
{-# INLINE bindUSM #-}
bindUSM (USM f) k = USM $ \us ->
  case f us of
    (# a, us' #) -> unUSM (k a) us'

pureUSM :: a -> UniqSM a
{-# INLINE pureUSM #-}
pureUSM x = USM $ \us -> (# x, us #)

apUSM :: UniqSM (a -> b) -> UniqSM a -> UniqSM b
-- Don't inline this one? GHC doesn't.
apUSM (USM f) (USM x) = USM $ \us0 ->
  case f us0 of
    (# ff, us1 #) -> case x us1 of
      (# xx, us2 #) -> (# ff xx, us2 #)

fmapUSM :: (a -> b) -> UniqSM a -> UniqSM b
{-# INLINE fmapUSM #-}
fmapUSM f (USM x) = USM $ \us ->
  case x us of
    (# x', us' #) -> (# f x', us' #)

-- lift a UniqSM computation to a lazy function.
liftUSM :: UniqSM a -> UniqSupply -> (a, UniqSupply)
liftUSM (USM f) us = case f us of (# a, us' #) -> (a, us')

instance MonadFix UniqSM where
  mfix m = USM $ \us -> let (a, us') = liftUSM (m a) us in (# a, us' #)

getUs :: UniqSM UniqSupply
getUs = USM $ \us -> case splitUniqSupply us of (us1, us2) -> (# us1, us2 #)

getUniqueUs :: UniqSM Unique
getUniqueUs = USM $ \us -> case takeUniqFromSupply us of (u,us') -> (# u, us' #)

getUniques :: UniqSM [Unique]
getUniques = USM $ \us ->
  case splitUniqSupply us of
    (us1, us2) -> (# uniqsFromSupply us1, us2 #)

class Monad m => MonadUnique m where
  getUniqueSupplyM :: m UniqSupply
  getUniqueM :: m Unique
  getUniquesM :: m [Unique]

  -- The default definition is correct, but generates and throws away
  -- an extra unique. Try to provide an explicit definition using
  -- 'takeUniqFromSupply' if possible.
  getUniqueM  = extractUniqFromSupply <$> getUniqueSupplyM
  -- This one has the same problem, but only generates an extra before
  -- the first one in the list. The rest are fine.
  getUniquesM = uniqsFromSupply       <$> getUniqueSupplyM

instance MonadUnique UniqSM where
  getUniqueSupplyM = getUs

showUnique :: Unique -> String
showUnique u = tag : base62repr untagged where
  (tag, untagged) = unpackUnique u

pprUnique :: Unique -> Doc
pprUnique u = text $ T.pack $ showUnique u

instance Outputable Unique where
  ppr = pprUnique

-- | Print a number in base 62 with characters [a-zA-Z0-9].
-- The input must be positive.
base62repr :: Int -> String
base62repr n_ = go n_ ""
  where
    go n cs
      | n < 62 
      = let !c = chooseChar62 n in c:cs
      | otherwise
      = let (!q, r) = n `quotRem` 62
            !c = chooseChar62 r
        in go q (c : cs)

    chooseChar62 :: Int -> Char
    {-# INLINE chooseChar62 #-}
    chooseChar62 (I# n) = C# (indexCharOffAddr# chars62 n)
    chars62 = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"#
