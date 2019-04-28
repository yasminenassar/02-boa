--------------------------------------------------------------------------------
-- | This module contains the code for converting an `Expr` to a "A-Normal" form.
--------------------------------------------------------------------------------

module Language.Boa.Normalizer ( anormal ) where

import           Language.Boa.Types

import Data.List (isPrefixOf)
import Data.Monoid
import Control.Arrow

type Binds a = [(Bind a, (AnfExpr a, a))]

--------------------------------------------------------------------------------
-- | Convert an Expr into A-Normal Form
--------------------------------------------------------------------------------
anormal :: Expr SourceSpan -> AnfExpr SourceSpan
--------------------------------------------------------------------------------
anormal = anfValidation >>> snd . (anf 0)


--------------------------------------------------------------------------------
-- | `anf i e` takes as input a "start" counter `i` and expression `e` and
--   returns an output `(i', e')` where
--   * `i'` is the output counter (i.e. i' - i) anf-variables were generated,
--   * `e'` is equivalent to `e` but is in A-Normal Form.
--------------------------------------------------------------------------------
anf :: Int -> Expr a -> (Int, AnfExpr a)
--------------------------------------------------------------------------------
anf i (Number n l)      = (i, Number n l)

anf i (Id     x l)      = (i, Id     x l)

anf i (Let x e b l)     = error "TBD:anf:let"

anf i (Prim1 o e l)     = (i', Prim1 o e' l)
  where
    (i', e') = anf i e

anf i (Prim2 o e1 e2 l) = (i'', stitch (bs2++bs1) (Prim2 o v1 v2 l))
  where
    (i', bs1, v1) = imm i e1
    (i'', bs2, v2) = imm i' e2

anf i (If c e1 e2 l)    = (i''', If c' e1' e2' l)
  where
    (i'  , c')  = anf i   c
    (i'' , e1') = anf i'  e1
    (i''', e2') = anf i'' e2


mkLet :: Binds a -> AnfExpr a -> AnfExpr a
mkLet [] e = e
mkLet ((b, (e1, a)):bs) e = Let b e1 (mkLet bs e) a  

--------------------------------------------------------------------------------
-- | `stitch bs e` takes a "context" `bs` which is a list of temp-vars and their
--   definitions, and an expression `e` that uses the temp-vars in `bs` and glues
--   them together into a `Let` expression.
--   NOTE: the binders are in reverse order.
--------------------------------------------------------------------------------
stitch :: Binds a -> AnfExpr a -> AnfExpr a
--------------------------------------------------------------------------------
stitch bs e = bindsExpr [ (x, xe) | (x, (xe, _)) <- reverse bs] e (getLabel e)

--------------------------------------------------------------------------------
-- | `imms i es` takes as input a "start" counter `i` and expressions `es`, and
--   and returns an output `(i', bs, es')` where
--   * `i'` is the output counter (i.e. i'- i) anf-variables were generated
--   * `bs` are the temporary binders needed to convert `es` to immediate vals
--   * `es'` are the immediate values  equivalent to es
--------------------------------------------------------------------------------
imms :: Int -> [Expr a] -> (Int, Binds a, [ImmExpr a])
--------------------------------------------------------------------------------
imms i []           = (i, [], [])
imms i (e:es)       = (i'', bs' ++ bs, e' : es' )
  where
    (i' , bs , e' ) = imm  i  e
    (i'', bs', es') = imms i' es
--------------------------------------------------------------------------------
-- | `imm i e` takes as input a "start" counter `i` and expression `e` and
--   returns an output `(i', bs, e')` where
--   * `i'` is the output counter (i.e. i' - i) anf-variables were generated,
--   * `bs` are the temporary binders needed to render `e` in ANF, and
--   * `e'` is an `imm` value (Id or Number) equivalent to `e`.
--------------------------------------------------------------------------------
imm :: Int -> Expr a -> (Int, Binds a, ImmExpr a)
--------------------------------------------------------------------------------
imm i (Number n l)      = (i, [], Number n l)

imm i (Id x l)          = (i, [], Id x l)

imm i e@(Prim1 _ _ l)   = immExp i e l

imm i (Prim2 o e1 e2 l) = (i''', (newBind, (Prim2 o v1 v2 l, l)):bs2++bs1, mkId newBind l)
  where
    (i', bs1, v1)       = imm i e1
    (i'', bs2, v2)      = imm i' e2
    (i''', newBind)     = fresh l i''  

imm i e@(If _ _ _  l)   = immExp i e l

imm i e@(Let _ _ _ l)   = immExp i e l


immExp :: Int -> Expr a -> a -> (Int, Binds a, ImmExpr a)
immExp i e l  = (i'', bs, mkId v l)
  where
    (i' , e') = anf i e
    (i'', v)  = fresh l i'
    bs        = [(v, (e', l))]

mkId :: Bind a -> a -> Expr a
mkId x l = Id (bindId x) l

--------------------------------------------------------------------------------
-- | `fresh i` returns a temp-var named `i` and "increments" the counter
--------------------------------------------------------------------------------
fresh :: a -> Int -> (Int, Bind a)
--------------------------------------------------------------------------------
fresh l i = (i + 1, Bind x l)
  where
    x     = "anf" ++ show i

anfValidation :: Bare -> Bare
anfValidation e =
  case getAlt $ go e of
    Nothing  -> e
    (Just l) -> panic "Variables that start with \"anf\" is not allowed" l

  where
    go (Let b e1 e2 _)   = mconcat $ chk b : (go <$> [e1, e2])
    go (Prim1 _ e _)     = go e
    go (Prim2 _ e1 e2 _) = mconcat $ go <$> [e1, e2]
    go (If c e1 e2 _)    = mconcat $ go <$> [c, e1, e2]
    go (Number _ _)      = Alt Nothing
    go (Id _ _)          = Alt Nothing

    chk :: BareBind -> Alt Maybe SourceSpan
    chk b =
      if   "anf" `isPrefixOf` (bindId b)
      then Alt (Just (sourceSpan b))
      else Alt Nothing
