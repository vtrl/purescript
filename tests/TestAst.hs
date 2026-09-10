{-# LANGUAGE TypeApplications #-}
module TestAst where

import Protolude hiding (Constraint, Type, (:+))

import Control.Lens ((+~))
import Control.Monad.Except qualified as Except
import Control.Monad.State.Strict qualified as State
import Control.Newtype (ala')
import Data.Map qualified as M
import Generic.Random (genericArbitraryRecG, genericArbitraryUG, listOf', uniform, withBaseCase, (:+)(..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Arbitrary(..), Gen, Property, Testable, counterexample, forAllShrink, subterms, (===))

import Language.PureScript.AST.SourcePos (SourcePos(..), SourceSpan(..))
import Language.PureScript.Comments (Comment(..))
import Language.PureScript.Environment qualified as E
import Language.PureScript.Errors (ErrorMessage(..), ErrorMessageHint(..), MultipleErrors(..), SimpleErrorMessage(..))
import Language.PureScript.Label (Label(..))
import Language.PureScript.Names (pattern ByNullSourcePos, ModuleName(..), OpName(..), OpNameType(..), ProperName(..), ProperNameType(..), Qualified(..), QualifiedBy(..))
import Language.PureScript.PSString (PSString)
import Language.PureScript.TypeChecker.Monad (CheckState(..), Substitution(..), UnkLevel(..), emptyCheckState)
import Language.PureScript.TypeChecker.Unify (unifyTypes)
import Language.PureScript.Types (Constraint, ConstraintData, SkolemScope(..), Type(..), TypeVarVisibility(..), WildcardData, annForType, everythingOnTypes, everythingWithContextOnTypes, everywhereOnTypes, everywhereOnTypesM, everywhereOnTypesTopDownM, getAnnForType)

spec :: Spec
spec = do
  describe "Language.PureScript.Types" $ do
    describe "everywhereOnTypes" $ do
      everywhereOnTypesSpec everywhereOnTypes
    describe "everywhereOnTypesM" $ do
      everywhereOnTypesSpec $ ala' Identity everywhereOnTypesM
    describe "everywhereOnTypesTopDownM" $ do
      everywhereOnTypesSpec $ ala' Identity everywhereOnTypesTopDownM
    describe "everythingOnTypes" $ do
      everythingOnTypesSpec everythingOnTypes
    describe "everythingWithContextOnTypes" $ do
      everythingOnTypesSpec $ \f g -> everythingWithContextOnTypes () [] f $ \s -> (s, ) . g
  describe "Language.PureScript.TypeChecker.Unify.unifyTypes" $ do
    let
      ann n = (SourceSpan "Leaves.purs" (SourcePos n 3) (SourcePos n 17), [LineComment (show n)])
      ctor mn = Qualified (ByModuleName (ModuleName mn)) (ProperName "Leaf")
      initial = (emptyCheckState E.initEnvironment)
        { checkNextType = 241
        , checkNextSkolem = 37
        , checkNextSkolemScope = 53
        , checkCurrentModule = Just (ModuleName "Leaves")
        , checkHints = [ErrorInModule (ModuleName "Outer")]
        , checkSubstitution = Substitution
            (M.fromList [(71, TUnknown (ann 8) 73), (73, E.tyInt)])
            (M.fromList [(89, (UnkLevel (pure 89), E.kindType)), (97, (UnkLevel (pure 97), E.kindType))])
            (M.fromList [(71, "kind"), (89, "left"), (97, "right")])
        }
      runUnify left right = State.runState (Except.runExceptT (unifyTypes left right)) initial

      -- CheckState has no Eq/Show, and Type's Eq omits annotations. Include
      -- every state field and use derived Show for annotation-sensitive checks.
      stateContents :: CheckState -> Text
      stateContents CheckState{..} = show
        ( checkEnv, checkNextType, checkNextSkolem, checkNextSkolemScope
        , checkCurrentModule, checkCurrentModuleImports
        , substType checkSubstitution, substUnsolved checkSubstitution, substNames checkSubstitution
        , checkHints, checkConstructorImportsForCoercible
        )

    it "preserves the entire state for equal constructors, variables, strings and integers with different annotations" $ do
      let pairs =
            [ (TypeConstructor (ann 1) (ctor "Left"), TypeConstructor (ann 9) (ctor "Left"))
            , (TypeVar (ann 2) "alpha17", TypeVar (ann 10) "alpha17")
            , (TypeLevelString (ann 3) "leaf-17", TypeLevelString (ann 11) "leaf-17")
            , (TypeLevelInt (ann 4) (-173), TypeLevelInt (ann 12) (-173))
            ]
      for_ pairs $ \(left, right) -> do
        let (result, finalState) = runUnify left right
        show result `shouldBe` ("Right ()" :: Text)
        stateContents finalState `shouldBe` stateContents initial

    it "retains the exact error contents and unification hint for unequal leaves" $ do
      let pairs =
            [ (TypeConstructor (ann 1) (ctor "Left"), TypeConstructor (ann 9) (ctor "Right"))
            , (TypeVar (ann 2) "alpha17", TypeVar (ann 10) "alpha71")
            , (TypeLevelString (ann 3) "leaf-17", TypeLevelString (ann 11) "leaf-71")
            , (TypeLevelInt (ann 4) (-173), TypeLevelInt (ann 12) 173)
            , (Skolem (ann 5) "same" Nothing 59 (SkolemScope 23), Skolem (ann 13) "same" Nothing 61 (SkolemScope 23))
            ]
      for_ pairs $ \(left, right) -> do
        let
          (result, _) = runUnify left right
          expected = Left (MultipleErrors [ErrorMessage [ErrorUnifyingTypes left right] (TypesDoNotUnify left right)])
            :: Either MultipleErrors ()
        show result `shouldBe` (show expected :: Text)

    it "uses only skolem identity even with different names, scopes and a nested unknown kind" $ do
      let
        left = Skolem (ann 1) "left"
          (Just (KindApp (ann 2) (TUnknown (ann 3) 71) (TUnknown (ann 4) 89))) 59 (SkolemScope 23)
        right = Skolem (ann 9) "right" Nothing 59 (SkolemScope 91)
        (result, finalState) = runUnify left right
      show result `shouldBe` ("Right ()" :: Text)
      stateContents finalState `shouldBe` stateContents initial

    it "still descends into applications and solves unknowns on either side" $ do
      let
        left = TypeApp (ann 1) (TUnknown (ann 2) 89) (E.tyInt $> ann 3)
        right = TypeApp (ann 4) (E.tyString $> ann 5) (TUnknown (ann 6) 97)
        (result, finalState) = runUnify left right
        expected = initial
          { checkSubstitution = (checkSubstitution initial)
              { substType = M.fromList
                  [(71, TUnknown (ann 8) 73), (73, E.tyInt), (89, E.tyString $> ann 5), (97, E.tyInt $> ann 3)]
              }
          }
      show result `shouldBe` ("Right ()" :: Text)
      stateContents finalState `shouldBe` stateContents expected

    it "retains outer and inner hints when a non-leaf unification fails after equal leaves" $ do
      let
        left = TypeApp (ann 1) (TypeConstructor (ann 2) (ctor "Left")) (TypeVar (ann 3) "alpha17")
        right = TypeApp (ann 4) (TypeConstructor (ann 5) (ctor "Left")) (TypeVar (ann 6) "alpha71")
        (result, _) = runUnify left right
        expected = Left (MultipleErrors [ErrorMessage
          [ErrorUnifyingTypes left right, ErrorUnifyingTypes (TypeVar (ann 3) "alpha17") (TypeVar (ann 6) "alpha71")]
          (TypesDoNotUnify (TypeVar (ann 3) "alpha17") (TypeVar (ann 6) "alpha71"))])
          :: Either MultipleErrors ()
      show result `shouldBe` (show expected :: Text)

everywhereOnTypesSpec :: ((Type Int -> Type Int) -> Type Int -> Type Int) -> Spec
everywhereOnTypesSpec everywhereOnTypesUnderTest = do
  it "should visit each type once" $
    forAllShrink (genTypeAnnotatedWith (pure 0) (pure 1)) subterms $ \t ->
      all (== 1) `isSatisfiedBy` everywhereOnTypesUnderTest (annForType +~ 1) t

everythingOnTypesSpec :: (([Int] -> [Int] -> [Int]) -> (Type Int -> [Int]) -> Type Int -> [Int]) -> Spec
everythingOnTypesSpec everythingOnTypesUnderTest = do
  it "should visit each type once" $
    forAllShrink (genTypeAnnotatedWith (pure 1) (pure 0)) subterms $ \t ->
      everythingOnTypesUnderTest (++) (pure . getAnnForType) t ===
        filter (== 1) (toList t)


infixr 0 `isSatisfiedBy`
isSatisfiedBy :: forall a p. Show a => Testable p => (a -> p) -> a -> Property
isSatisfiedBy = liftA2 counterexample show

genTypeAnnotatedWith :: forall a. Gen a -> Gen a -> Gen (Type a)
genTypeAnnotatedWith genTypeAnn genConstraintAnn = genType where
  generatorEnvironment
    =  genConstraint
    :+ maybeOf genConstraintData
    :+ Label <$> genPSString
    :+ genPSString
    :+ genQualified (OpName @'TypeOpName)
    :+ genQualified (ProperName @'ClassName)
    :+ genQualified (ProperName @'TypeName)
    :+ genSkolemScope
    :+ maybeOf genSkolemScope
    :+ genText
    :+ listOf' (listOf' genText)
    :+ maybeOf genText
    :+ genType
    :+ listOf' genType
    :+ maybeOf genType
    :+ genWildcardData
    :+ genVisibility

  genConstraint :: Gen (Constraint a)
  genConstraint = genericArbitraryUG (genConstraintAnn :+ generatorEnvironment)

  genConstraintData :: Gen ConstraintData
  genConstraintData = genericArbitraryUG generatorEnvironment

  genQualified :: forall b. (Text -> b) -> Gen (Qualified b)
  genQualified ctor = Qualified ByNullSourcePos . ctor <$> genText

  genSkolemScope :: Gen SkolemScope
  genSkolemScope = SkolemScope <$> arbitrary

  genType :: Gen (Type a)
  genType = genericArbitraryRecG (genTypeAnn :+ generatorEnvironment) uniform `withBaseCase` (TypeVar <$> genTypeAnn <*> genText)

  genWildcardData :: Gen WildcardData
  genWildcardData = genericArbitraryUG genText

  maybeOf :: forall b. Gen b -> Gen (Maybe b)
  maybeOf = genericArbitraryUG

  genText :: Gen Text
  genText = pure "x" -- Feel free to make this random if it matters at some point.

  genPSString :: Gen PSString
  genPSString = pure "x" -- Ditto.

  genVisibility :: Gen TypeVarVisibility
  genVisibility = pure TypeVarInvisible
