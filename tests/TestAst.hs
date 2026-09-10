{-# LANGUAGE TypeApplications #-}
module TestAst where

import Protolude hiding (Constraint, Type, (:+))

import Control.Lens ((+~))
import Control.Monad.State.Strict qualified as State
import Control.Newtype (ala')
import Data.Map qualified as M
import Generic.Random (genericArbitraryRecG, genericArbitraryUG, listOf', uniform, withBaseCase, (:+)(..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Arbitrary(..), Gen, Property, Testable, counterexample, forAllShrink, subterms, (===))

import Language.PureScript.AST.SourcePos (SourcePos(..), SourceSpan(..))
import Language.PureScript.Comments (Comment(..))
import Language.PureScript.Environment (initEnvironment)
import Language.PureScript.Label (Label(..))
import Language.PureScript.Names (pattern ByNullSourcePos, OpName(..), OpNameType(..), ProperName(..), ProperNameType(..), Qualified(..))
import Language.PureScript.PSString (PSString)
import Language.PureScript.TypeChecker.Monad (CheckState(..), Substitution(..), emptyCheckState)
import Language.PureScript.TypeChecker.Unify (varIfUnknown)
import Language.PureScript.Types (Constraint(..), ConstraintData, SkolemScope(..), Type(..), TypeVarVisibility(..), WildcardData, annForType, everythingOnTypes, everythingWithContextOnTypes, everywhereOnTypes, everywhereOnTypesM, everywhereOnTypesTopDownM, getAnnForType)

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
  describe "Language.PureScript.TypeChecker.Unify.varIfUnknown" $ do
    let
      ann n = (SourceSpan "Generalize.purs" (SourcePos n 3) (SourcePos n 19), [LineComment (show n)])
      initial = (emptyCheckState initEnvironment)
        { checkNextType = 321
        , checkSubstitution = (checkSubstitution (emptyCheckState initEnvironment))
            { substNames = M.fromList [(103, "k"), (42, "a9"), (88, "outside"), (65, "")] }
        }

    it "preserves supplied binding order, generalized kinds, names and annotations without changing state" $ do
      let
        kind = TypeConstructor (ann 2) (Qualified ByNullSourcePos (ProperName "Type"))
        body = KindedType (ann 1)
          (TypeApp (ann 5) (TUnknown (ann 6) 42) (TUnknown (ann 7) 88))
          (TUnknown (ann 8) 17)
        expected = ForAll (ann 1) TypeVarInvisible "k103" (Just kind)
          (ForAll (ann 1) TypeVarInvisible "t17" (Just (TypeVar (ann 3) "k103"))
            (ForAll (ann 1) TypeVarInvisible "a942" (Just (TypeVar (ann 4) "t17"))
              (KindedType (ann 1)
                (TypeApp (ann 5) (TypeVar (ann 6) "a942") (TypeVar (ann 7) "outside88"))
                (TypeVar (ann 8) "t17"))
              Nothing)
            Nothing)
          Nothing
        (actual, finalState) = State.runState
          (varIfUnknown [(103, kind), (17, TUnknown (ann 3) 103), (42, TUnknown (ann 4) 17)] body)
          initial
      -- Type's Eq ignores annotations and forall visibility; Show includes both.
      show actual `shouldBe` (show expected :: Text)
      substNames (checkSubstitution finalState) `shouldBe` substNames (checkSubstitution initial)
      substType (checkSubstitution finalState) `shouldBe` M.empty
      substUnsolved (checkSubstitution finalState) `shouldBe` M.empty
      checkNextType finalState `shouldBe` 321

    it "replaces unquantified unknowns in nested kinds, constraints, rows and operator syntax" $ do
      let
        cls = Qualified ByNullSourcePos (ProperName "C")
        body = ForAll (ann 1) TypeVarVisible "kept" (Just (TUnknown (ann 2) 103))
          (ConstrainedType (ann 3)
            (Constraint (ann 4) cls [TUnknown (ann 5) 17] [TUnknown (ann 6) 42] Nothing)
            (RCons (ann 7) (Label "field")
              (Skolem (ann 8) "s" (Just (TUnknown (ann 9) 88)) 23 (SkolemScope 31))
              (ParensInType (ann 10)
                (BinaryNoParensType (ann 11) (TUnknown (ann 12) 42) (TUnknown (ann 13) 17)
                  (KindApp (ann 14) (REmpty (ann 15)) (TUnknown (ann 16) 103))))))
          (Just (SkolemScope 29))
        expected = ForAll (ann 1) TypeVarVisible "kept" (Just (TypeVar (ann 2) "k103"))
          (ConstrainedType (ann 3)
            (Constraint (ann 4) cls [TypeVar (ann 5) "t17"] [TypeVar (ann 6) "a942"] Nothing)
            (RCons (ann 7) (Label "field")
              (Skolem (ann 8) "s" (Just (TypeVar (ann 9) "outside88")) 23 (SkolemScope 31))
              (ParensInType (ann 10)
                (BinaryNoParensType (ann 11) (TypeVar (ann 12) "a942") (TypeVar (ann 13) "t17")
                  (KindApp (ann 14) (REmpty (ann 15)) (TypeVar (ann 16) "k103"))))))
          (Just (SkolemScope 29))
        actual = State.evalState (varIfUnknown [] body) initial
      show actual `shouldBe` (show expected :: Text)

    it "keeps existing type variables and distinguishes an empty stored prefix from the default" $ do
      let
        body = TypeApp (ann 1) (TypeVar (ann 2) "t17")
          (TypeApp (ann 3) (TUnknown (ann 4) 65) (TUnknown (ann 5) 17))
        expected = TypeApp (ann 1) (TypeVar (ann 2) "t17")
          (TypeApp (ann 3) (TypeVar (ann 4) "65") (TypeVar (ann 5) "t17"))
        actual = State.evalState (varIfUnknown [] body) initial
      show actual `shouldBe` (show expected :: Text)

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
