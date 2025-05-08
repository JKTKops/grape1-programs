-- Prettyprinting for Core expressions.
module Core.Ppr
  ( pprCoreExpr, pprCoreBinding, pprCoreBindings
  ) where

import Core.Expr
import Outputable

instance OutputableBinder b => Outputable (Expr b) where
  ppr = pprCoreExpr

instance OutputableBinder b => Outputable (Bind b) where
  ppr = pprCoreBinding

pprCoreExpr     = undefined
pprCoreBinding  = undefined
pprCoreBindings = undefined
