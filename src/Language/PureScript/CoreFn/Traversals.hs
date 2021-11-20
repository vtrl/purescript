-- |
-- CoreFn traversal helpers
--
module Language.PureScript.CoreFn.Traversals where

import Prelude.Compat

import Control.Arrow (Kleisli(..), second, (***), (+++))

import Language.PureScript.AST.Literals
import Language.PureScript.CoreFn.Binders
import Language.PureScript.CoreFn.Expr

everywhereOnValues :: (Bind a -> Bind a) ->
                      (Expr a -> Expr a) ->
                      (Binder a -> Binder a) ->
                      (Bind a -> Bind a, Expr a -> Expr a, Binder a -> Binder a)
everywhereOnValues f g h = (f', g', h')
  where
  f' (NonRec a name e) = f (NonRec a name (g' e))
  f' (Rec es) = f (Rec (map (second g') es))

  g' (Literal ann e) = g (Literal ann (handleLiteral g' e))
  g' (Accessor ann prop e) = g (Accessor ann prop (g' e))
  g' (ObjectUpdate ann obj vs) = g (ObjectUpdate ann (g' obj) (map (fmap g') vs))
  g' (Abs ann name e) = g (Abs ann name (g' e))
  g' (App ann v1 v2) = g (App ann (g' v1) (g' v2))
  g' (Case ann vs alts) = g (Case ann (map g' vs) (map handleCaseAlternative alts))
  g' (Let ann ds e) = g (Let ann (map f' ds) (g' e))
  g' e = g e

  h' (LiteralBinder a b) = h (LiteralBinder a (handleLiteral h' b))
  h' (NamedBinder a name b) = h (NamedBinder a name (h' b))
  h' (ConstructorBinder a q1 q2 bs) = h (ConstructorBinder a q1 q2 (map h' bs))
  h' b = h b

  handleCaseAlternative ca =
    ca { caseAlternativeBinders = map h' (caseAlternativeBinders ca)
       , caseAlternativeResult = (map (g' *** g') +++ g') (caseAlternativeResult ca)
       }

  handleLiteral :: (a -> a) -> Literal a -> Literal a
  handleLiteral i (ArrayLiteral ls) = ArrayLiteral (map i ls)
  handleLiteral i (ObjectLiteral ls) = ObjectLiteral (map (fmap i) ls)
  handleLiteral _ other = other

everywhereOnValuesM :: Monad m =>
  (Bind a -> m (Bind a)) ->
  (Expr a -> m (Expr a)) ->
  (Binder a -> m (Binder a)) ->
  (Bind a -> m (Bind a), Expr a -> m (Expr a), Binder a -> m (Binder a))
everywhereOnValuesM f g h = (f', g', h')
  where
  f' (NonRec a name e) = (NonRec a name <$> g' e) >>= f
  f' (Rec es) = (Rec <$> traverse (secondM g') es) >>= f

  g' (Literal ann e) = Literal ann <$> handleLiteral g' e >>= g
  g' (Accessor ann prop e) = Accessor ann prop <$> g' e >>= g
  g' (ObjectUpdate ann obj vs) = ObjectUpdate ann <$> g' obj <*> (traverse (traverse g') vs) >>= g
  g' (Abs ann name e) = Abs ann name <$> g' e >>= g
  g' (App ann v1 v2) = App ann <$> g' v1 <*> g' v2 >>= g
  g' (Case ann vs alts) = Case ann <$> traverse g' vs <*> traverse handleCaseAlternatives alts >>= g
  g' (Let ann ds e) = Let ann <$> traverse f' ds <*> g' e >>= g
  g' e = g e

  h' (LiteralBinder a b) = LiteralBinder a <$> handleLiteral h' b >>= h
  h' (NamedBinder a name b) = NamedBinder a name <$> h' b >>= h
  h' (ConstructorBinder a q1 q2 bs) = ConstructorBinder a q1 q2 <$> traverse h' bs >>= h
  h' b = h b

  secondM fn (a, b) = (a,) <$> fn b

  handleCaseAlternatives ca = do
    let g'' = Kleisli g'
        g''' = Kleisli (traverse (runKleisli (g'' *** g''))) +++ g''
    caseAlternativeBinders' <- traverse h' (caseAlternativeBinders ca)
    caseAlternativeResult' <- runKleisli g''' (caseAlternativeResult ca)
    pure $ ca { caseAlternativeBinders = caseAlternativeBinders'
              , caseAlternativeResult = caseAlternativeResult'
              }

  handleLiteral :: Monad m => (a -> m a) -> Literal a -> m (Literal a)
  handleLiteral i (ArrayLiteral ls) = ArrayLiteral <$> traverse i ls
  handleLiteral i (ObjectLiteral ls) = ObjectLiteral <$> (traverse (traverse i) ls)
  handleLiteral _ other = pure other
