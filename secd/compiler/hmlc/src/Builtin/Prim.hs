{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- | Stuff useful for defining builtins.
module Builtin.Prim
  ( module Builtin.Prim
  ) where

import Builtin.Keys
import Core.Types
import Types.Unique

import Data.Char
import Data.Text qualified as T

mkTemplateTyVars :: [Kind] -> [TyVar]
mkTemplateTyVars = mkTemplateTyVarsFrom 0

mkTemplateTyVarsFrom :: Int -> [Kind] -> [TyVar]
mkTemplateTyVarsFrom n kinds =
  [ mkTyVar name kind
  | (kind, ix) <- zip kinds [0..],
    let ch_ord = ix + ord 'a'
        name_str | ch_ord <= ord 'z' = [chr ch_ord]
                 | otherwise         = 't':show ix
        name = mkTvName (ix + n) (T.pack name_str)
  ]

mkTvName :: Int -> T.Text -> Name
mkTvName u t = mkInternalName t (mkAlphaTyVarKey u)

mkTemplateTyConBinders :: [Kind] -> [TyVar]
mkTemplateTyConBinders = mkTemplateTyVars

alphaTyVars :: [TyVar]
alphaTyVars = mkTemplateTyVars $ repeat Star

alphaTyVar, betaTyVar, gammaTyVar, deltaTyVar :: TyVar
(alphaTyVar:betaTyVar:gammaTyVar:deltaTyVar : _) = alphaTyVars

alphaTys :: [Type]
alphaTys = mkTyVarTys alphaTyVars

alphaTy, betaTy, gammaTy, deltaTy :: Type
(alphaTy:betaTy:gammaTy:deltaTy:_) = alphaTys
