module Outputable
  ( -- * class
    Outputable (..)

    -- * 'Doc's, styles, and contexts
  , Doc, DocContext(DocContext), PprStyle(..), Depth(..)
  , isPprUser, isPprDump, isPprCode

    -- * Tools for constructing docs and constant docs
  , text
  , enlisted, tupled
  , parens, brackets, braces, angles, quotes, doubleQuotes, align, group
  , parensIf, semi, comma, colon, dcolon, space, equals, dot, vbar
  , arrow, larrow, darrow, lambda, lparen, rparen, lbrace, rbrace
  , lbrack, rbrack, underscore, line, emptyDoc
  , plural, surround, punctuate, hsep, vsep, fillSep, sep, hcat, vcat
  , fillCat, cat
  , concatWith, (<+>)

    -- * Manipulating indentation
  , indent, nest, hang

    -- * Choosing settings or making decisions based on them
  , viewPprDebug, ifPprDebug, withPprDebug, viewPprStyle, withPprStyle

    -- * Depth manipulation
  , pprDeeper, pprDeeperList, pprSetDepth

    -- * Printing 'Doc's
  , output, putDoc, putDocWith, defaultUserContext, defaultDebugContext
  , renderDoc, renderDocWith
  ) where

-- For now, we just re-export Prettyprinter, but someday we will probably want
-- to define replacement functions for a more involved doc type that is
-- actually a function from some flags or other formatting settings to a Doc.

import Prettyprinter qualified as P
import Prettyprinter.Render.Text qualified as P
import Data.Text (Text)
import Data.Functor.Identity (Identity(..))

data PprStyle
  = PprUser Depth
  | PprDump -- ^ for dump options, less verbose than debug but not for end users.
  | PprCode -- ^ Render something as code
            -- (Used for things that can be output directly to bytecode).

isPprUser :: PprStyle -> Bool
isPprUser PprUser{} = True
isPprUser _ = False

isPprDump :: PprStyle -> Bool
isPprDump PprDump = True
isPprDump _ = False

isPprCode :: PprStyle -> Bool
isPprCode PprCode = True
isPprCode _ = False

data Depth
  = AllTheWay
  | PartWay Int
  | DefaultDepth -- ^ Use 'defaultDepth' as the depth.

data DocContext
  = DocContext
    { style :: !PprStyle
    , debug :: !Bool
    , defaultDepth :: !Int
    }

type PDoc = P.Doc ()
newtype Doc = Doc {runDoc :: DocContext -> P.Doc ()}

output :: Outputable a => a -> IO ()
output = putDoc . ppr

putDoc :: Doc -> IO ()
putDoc = putDocWith defaultUserContext

putDocWith :: DocContext -> Doc -> IO ()
putDocWith ctx d = (P.putDoc $ runDoc d ctx) >> putStrLn ""

renderDoc :: Doc -> Text
renderDoc = renderDocWith defaultUserContext

renderDocWith :: DocContext -> Doc -> Text
renderDocWith ctx d = 
  P.renderStrict $ P.layoutPretty P.defaultLayoutOptions $ runDoc d ctx

defaultUserContext :: DocContext
defaultUserContext = DocContext
  { style = PprUser DefaultDepth
  , debug = False
  , defaultDepth = 5
  }

defaultDebugContext :: DocContext
defaultDebugContext = DocContext
  { style = PprDump
  , debug = True
  , defaultDepth = 100
  }

viewPprDebug :: (Bool -> Doc) -> Doc
viewPprDebug f = Doc $ \ctx ->
  runDoc (f (debug ctx)) ctx

ifPprDebug :: Doc -> Doc
ifPprDebug d = viewPprDebug $ \b -> if b then d else emptyDoc

withPprDebug :: Doc -> Doc
withPprDebug (Doc f) = Doc $ \ctx -> f ctx{debug=True}

viewPprStyle :: (PprStyle -> Doc) -> Doc
viewPprStyle f = Doc $ \ctx ->
  runDoc (f (style ctx)) ctx

withPprStyle :: PprStyle -> Doc -> Doc
withPprStyle sty (Doc f) = Doc $ \ctx -> f ctx{style=sty}

-- | If we're printing in User style, and the current remaining depth is 0,
-- then truncate the printing. Otherwise, print the 'Doc' as given.
pprDeeper :: Doc -> Doc
pprDeeper d = Doc $ \ctx -> case style ctx of
  PprUser depth ->
    let deeper 0 = P.pretty @Text "..."
        deeper n = runDoc d ctx{style = PprUser (PartWay (n-1))}
    in case depth of
      DefaultDepth -> deeper (defaultDepth ctx)
      PartWay n    -> deeper n
      AllTheWay    -> runDoc d ctx
  _ -> runDoc d ctx

-- | Truncate a list if it is longer than the current depth.
pprDeeperList :: ([Doc] -> Doc) -> [Doc] -> Doc
pprDeeperList f ds
  | null ds   = f []
  | otherwise = Doc work
  where
    work ctx@DocContext{style=PprUser depth}
      | DefaultDepth <- depth
      = work (ctx{style = PprUser (PartWay (defaultDepth ctx))})
      | PartWay 0 <- depth
      = P.pretty @Text "..."
      | PartWay n <- depth
      = let go _ [] = []
            go i (d:ds') | i >= n    = [text "..."]
                         | otherwise = d : go (i+1) ds'
        in runDoc (f (go 0 ds)) ctx{style=PprUser (PartWay (n-1))}
    work other_ctx = runDoc (f ds) other_ctx

pprSetDepth :: Depth -> Doc -> Doc
pprSetDepth depth doc = Doc $ \ctx ->
  case ctx of
    DocContext{style = PprUser _} ->
      runDoc doc ctx{style = PprUser depth}
    _ -> runDoc doc ctx

-- | Inject a PDoc to Doc
inj :: PDoc -> Doc
inj pd = Doc $ \_ -> pd

-- | Lift a simple PDoc function to a Doc function.
liftP :: (a -> PDoc) -> (a -> Doc)
liftP p = \x -> Doc $ \_ -> p x

-- | Lift a functorial PDoc transformer to a Doc transformer.
liftFPT :: Functor f => (f PDoc -> PDoc) -> (f Doc -> Doc)
liftFPT pt = \fx -> Doc $ \ctx -> pt (fmap (($ ctx) . runDoc) fx)

-- | Lift a PDoc transformer to a Doc transformer.
liftPT :: (PDoc -> PDoc) -> (Doc -> Doc)
liftPT pt = liftFPT (pt . runIdentity) . Identity

liftBinPT :: (PDoc -> PDoc -> PDoc) -> (Doc -> Doc -> Doc)
liftBinPT p = \x y -> Doc $ \ctx -> p (runDoc x ctx) (runDoc y ctx)

text :: Text -> Doc
text = ppr

class Outputable a where
  ppr :: a -> Doc
  pprList :: [a] -> Doc
  pprList xs = enlisted $ map ppr xs

-- | Format 'Doc's like a Haskell list, ala 'P.list'.
enlisted :: [Doc] -> Doc
enlisted = liftFPT P.list

-- | Format 'Doc's like a Haskell tuple, ala 'P.tupled'.
tupled :: [Doc] -> Doc
tupled = liftFPT P.tupled

parens, brackets, braces, quotes, doubleQuotes, angles,
  align, group :: Doc -> Doc
parens   = liftPT P.parens
brackets = liftPT P.parens
braces   = liftPT P.braces
quotes   = liftPT P.squotes
doubleQuotes = liftPT P.dquotes
angles   = liftPT P.angles
align    = liftPT P.align
group    = liftPT P.group

parensIf :: Bool -> Doc -> Doc
parensIf b = if b then parens else id

semi, comma, colon, dcolon, space, equals, dot, vbar, arrow, larrow, darrow,
  lambda, lparen, rparen, lbrack, rbrack, lbrace, rbrace, underscore,
  line, emptyDoc
  :: Doc
semi   = inj P.semi
comma  = inj P.comma
colon  = inj P.colon
dcolon = text "::"
space  = inj P.space
equals = inj P.equals
dot    = inj P.dot
vbar   = ppr '|'
arrow  = text "->"
larrow = text "<-"
darrow = text "=>"
lambda = ppr '\\'
lparen = inj P.lparen
rparen = inj P.rparen
lbrack = inj P.lbracket
rbrack = inj P.rbracket
lbrace = inj P.lbrace
rbrace = inj P.rbrace
underscore = ppr '_'
line       = inj P.line
emptyDoc   = inj P.emptyDoc

nest, hang, indent :: Int -> Doc -> Doc
nest n = liftPT (P.nest n)
hang n = liftPT (P.hang n)
indent n = liftPT (P.indent n)

instance Semigroup Doc where
  (<>) = liftBinPT (P.<>)

instance Monoid Doc where
  mempty = emptyDoc

(<+>) :: Doc -> Doc -> Doc
(<+>) = liftBinPT (P.<+>)

concatWith :: Foldable t => (Doc -> Doc -> Doc) -> t Doc -> Doc
concatWith f td
  | null td   = mempty
  | otherwise = foldr1 f td

hsep, vsep, fillSep, sep, hcat, vcat, fillCat, cat :: [Doc] -> Doc
hsep = liftFPT P.hsep
vsep = liftFPT P.vsep
fillSep = liftFPT P.fillSep
sep  = liftFPT P.sep
hcat = liftFPT P.hcat
vcat = liftFPT P.vcat
fillCat = liftFPT P.fillCat
cat  = liftFPT P.cat

punctuate :: Doc -> [Doc] -> [Doc]
punctuate punc = go where
  go [] = []
  go [d] = [d]
  go (d : ds) = (d <> punc) : go ds

plural :: (Num amount, Eq amount)
       => doc -> doc -> amount -> doc
plural one many n
  | n == 1    = one
  | otherwise = many

surround :: Doc -> Doc -> Doc -> Doc
surround x l r = l <> x <> r



---------------------------------------------------------------------
------ Instances
---------------------------------------------------------------------

instance Outputable Bool where
  ppr = liftP P.pretty

instance Outputable Char where
  ppr = liftP P.pretty

instance Outputable Int where
  ppr = liftP P.pretty

instance Outputable Integer where
  ppr = liftP P.pretty

instance Outputable () where
  ppr = liftP P.pretty -- note that the argument is not forced

instance Outputable Text where
  ppr = liftP P.pretty

instance Outputable a => Outputable [a] where
  ppr = pprList

instance Outputable a => Outputable (Maybe a) where
  ppr Nothing = mempty
  ppr (Just x) = ppr x

instance (Outputable a, Outputable b) => Outputable (a,b) where
  ppr (x,y) = tupled [ppr x, ppr y]

instance (Outputable a, Outputable b, Outputable c)
          => Outputable (a,b,c) where
  ppr (x,y,z) = tupled [ppr x, ppr y, ppr z]

instance Outputable PprStyle where
  ppr (PprUser {}) = text "user-style"
  ppr (PprCode {}) = text "code-style"
  ppr (PprDump {}) = text "dump-style"
