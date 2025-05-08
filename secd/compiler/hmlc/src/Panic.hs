module Panic (panic, sorry, assert) where

import Control.Exception ( throw, Exception )
import Data.Text qualified as T

import qualified Paths_hmlc as Paths
import GHC.Stack (HasCallStack, withFrozenCallStack)

data HmlcException
  = Panic T.Text
  | Sorry T.Text

instance Exception HmlcException

panic :: HasCallStack => T.Text -> a
panic msg = throw $ Panic msg

sorry :: T.Text -> a
sorry msg = throw $ Sorry msg

assert :: HasCallStack => Bool -> a -> a
{-# INLINE assert #-}
assert cond a =
  if not cond
    then withFrozenCallStack (panic "Assert failed!")
    else a

instance Show HmlcException where
  showsPrec _ = showHmlcException

showHmlcException :: HmlcException -> ShowS
showHmlcException = \case
  Panic s -> panicMsg $ shows s
  Sorry s -> sorryMsg $ shows s
  where
    sorryMsg :: ShowS -> ShowS
    sorryMsg s =
        showString "sorry! (unimplemented feature or known bug)\n"
      . showString ("  (HMLC version " ++ show Paths.version ++ "):\n\t")
      . s . showString "\n"

    panicMsg :: ShowS -> ShowS
    panicMsg s =
        showString "panic! (the 'impossible' happened)\n"
      . showString ("  (HMLC version " ++ show Paths.version ++ "):\n\t")
      . s . showString "\n"
