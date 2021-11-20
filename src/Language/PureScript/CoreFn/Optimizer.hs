module Language.PureScript.CoreFn.Optimizer (optimizeCoreFn) where

import Protolude hiding (Type)

import Control.Monad.Supply.Class (MonadSupply, fresh)
import Data.List (lookup)
import Language.PureScript.AST.Literals
import Language.PureScript.AST.SourcePos
import Language.PureScript.CoreFn.Ann
import Language.PureScript.CoreFn.Expr
import Language.PureScript.CoreFn.Module
import Language.PureScript.CoreFn.Traversals
import Language.PureScript.Names (Ident(..), ModuleName(..), Qualified(..))
import Language.PureScript.Label
import Language.PureScript.Types
import qualified Language.PureScript.Constants.Prim as C

-- |
-- CoreFn optimization pass.
--
optimizeCoreFn :: MonadSupply m => Module Ann -> m (Module Ann)
optimizeCoreFn m = do
  moduleDecls' <- optimizeModuleDecls $ moduleDecls m
  pure $ m {moduleDecls = moduleDecls'}

optimizeModuleDecls :: MonadSupply m => [Bind Ann] -> m [Bind Ann]
optimizeModuleDecls = traverse transformBinds
  where
  (transformBinds, _, _) = everywhereOnValuesM pure transformExprs pure
  transformExprs =
    pure . optimizeClosedRecordUpdate
      <=< pure . optimizeUnusedPartialFn
      <=< optimizeFnComposition
      <=< pure . optimizeDataFunctionApply


optimizeClosedRecordUpdate :: Expr Ann -> Expr Ann
optimizeClosedRecordUpdate ou@(ObjectUpdate a@(_, _, Just t, _) r updatedFields) =
  case closedRecordFields t of
    Nothing -> ou
    Just allFields -> Literal a (ObjectLiteral (map f allFields))
      where f (Label l) = case lookup l updatedFields of
              Nothing -> (l, Accessor (nullSourceSpan, [], Nothing, Nothing) l r)
              Just e -> (l, e)
optimizeClosedRecordUpdate e = e

-- | Return the labels of a closed record, or Nothing for other types or open records.
closedRecordFields :: Type a -> Maybe [Label]
closedRecordFields (TypeApp _ (TypeConstructor _ C.Record) row) =
  collect row
  where
    collect :: Type a -> Maybe [Label]
    collect (REmptyKinded _ _) = Just []
    collect (RCons _ l _ r) = (l :) <$> collect r
    collect _ = Nothing
closedRecordFields _ = Nothing

-- | See https://github.com/purescript/purescript/issues/3157
optimizeUnusedPartialFn :: Expr Ann -> Expr Ann
optimizeUnusedPartialFn (Let _
  [NonRec _ UnusedIdent _]
  (App _ (App _ (Var _ (Qualified _ UnusedIdent)) _) originalCoreFn)) =
  originalCoreFn
optimizeUnusedPartialFn e = e

-- | TODO: Fixup the annotations here.
optimizeDataFunctionApply :: Expr Ann -> Expr Ann
optimizeDataFunctionApply e = case e of
  (App a (App _ (Var _ (Qualified (Just (ModuleName dataFunction)) (Ident applyFn))) x) y)
    | dataFunction == "Data.Function" && applyFn == "apply" -> App a x y
    | dataFunction == "Data.Function" && applyFn == "applyFlipped" -> App a y x
  _ -> e

optimizeFnComposition :: MonadSupply m => Expr Ann -> m (Expr Ann)
optimizeFnComposition = \case
  (App a (App b (App _ (App _
   (Var _ (Qualified (Just (ModuleName "Control.Semigroupoid")) (Ident composeFn)))
   (Var _ (Qualified (Just (ModuleName "Control.Semigroupoid")) (Ident "semigroupoidFn"))))
    x) y) z)
    | composeFn == "compose" -> pure $ App a x (App b y z)
    | composeFn == "composeFlipped" -> pure $ App b y (App a x z)

  app@(App a (App _ (App _
       (Var _ (Qualified (Just (ModuleName "Control.Semigroupoid")) (Ident composeFn)))
       (Var _ (Qualified (Just (ModuleName "Control.Semigroupoid")) (Ident "semigroupoidFn"))))
        _) _)
    | composeFn == "compose" || composeFn == "composeFlipped" -> do
        n <- fresh
        pure $ Abs a (GenIdent Nothing n) (foldCompose a (Qualified Nothing (GenIdent Nothing n)) (collectCompose app))

  e -> pure e
  where
    foldCompose :: Ann -> Qualified Ident -> [Either (Expr Ann) (Expr Ann)] -> Expr Ann
    foldCompose a i = foldr (\fn acc -> App a (either identity identity fn) acc) (Var a i)

    collectCompose :: Expr Ann -> [Either (Expr Ann) (Expr Ann)]
    collectCompose = \case
      (App _ (App _ (App _
       (Var _ (Qualified (Just (ModuleName "Control.Semigroupoid")) (Ident composeFn)))
       (Var _ (Qualified (Just (ModuleName "Control.Semigroupoid")) (Ident "semigroupoidFn"))))
        x) y)
        | composeFn == "compose" -> mappend (collectCompose x) (collectCompose y)
        | composeFn == "composeFlipped" -> mappend (collectCompose y) (collectCompose x)

      e@App{} -> [Right e]

      e -> [Left e]
