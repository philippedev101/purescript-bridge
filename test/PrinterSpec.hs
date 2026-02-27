{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module PrinterSpec (spec) where

import           Data.Maybe (isJust, isNothing)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import           Language.PureScript.Bridge (bridgeSumType, buildBridge,
                                             defaultBridge, mkSumType)
import           Language.PureScript.Bridge.Printer (Module (PSModule, psModuleName, psImportLines, psQualifiedImports, psTypes),
                                                     constructorPattern,
                                                     constructor,
                                                     flattenTuple, hasUnderscore,
                                                     isEnum, mkPackageName,
                                                     renderText, sumTypeToModule,
                                                     sumTypesToModules, typeParams,
                                                     typeToImportLines,
                                                     unionImportLine,
                                                     unionImportLines,
                                                     unionModules)
import           Language.PureScript.Bridge.PSTypes (psInt, psString, psUnit)
import           Language.PureScript.Bridge.SumType (DataConstructor (..),
                                                     DataConstructorArgs (..),
                                                     ImportLine (..),
                                                     RecordEntry (..),
                                                     SumType (..),
                                                     importsFromList)
import           Language.PureScript.Bridge.TypeInfo (TypeInfo (..),
                                                      Language (PureScript))
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty (shouldBe, shouldSatisfy)
import           TestData (Foo, WeekInMonth)
import           Text.PrettyPrint.Leijen.Text (textStrict)

spec :: Spec
spec = do
    describe "renderText" $ do
        it "renders a simple doc to text" $
            renderText (textStrict "hello") `shouldBe` "hello"

        it "renders empty doc to empty text" $
            renderText mempty `shouldBe` ""

    describe "mkPackageName" $ do
        it "accepts valid alpha-only names" $
            isJust (mkPackageName "MyPackage") `shouldBe` True

        it "rejects names with digits" $
            isNothing (mkPackageName "package1") `shouldBe` True

        it "rejects names with special characters" $
            isNothing (mkPackageName "my-package") `shouldBe` True

        it "accepts empty string (vacuous truth: all isAlpha \"\" = True)" $
            isJust (mkPackageName "") `shouldBe` True

    describe "sumTypeToModule" $ do
        it "produces correct module name" $ do
            let st = bridgeSumType (buildBridge defaultBridge) (mkSumType @Foo)
                modules = sumTypeToModule Nothing st
            Map.size modules `shouldBe` 1
            let modName = head $ Map.keys modules
            modName `shouldBe` "TestData"

        it "uses package name prefix when provided" $ do
            let st = bridgeSumType (buildBridge defaultBridge) (mkSumType @Foo)
                modules = sumTypeToModule (mkPackageName "MyApp") st
                m = head $ Map.elems modules
            psModuleName m `shouldSatisfy` T.isPrefixOf "MyApp."

    describe "sumTypesToModules" $ do
        it "merges types with same module" $ do
            let bridge = buildBridge defaultBridge
                st1 = bridgeSumType bridge (mkSumType @Foo)
                st2 = bridgeSumType bridge (mkSumType @WeekInMonth)
                modules = sumTypesToModules Nothing [st1, st2]
            -- Both Foo and WeekInMonth are in TestData module
            Map.size modules `shouldBe` 1

    describe "unionModules" $ do
        it "merges import lines and types" $ do
            let m1 = mkTestModule "Test" [ImportLine "Data.Maybe" Nothing $ Set.singleton "Maybe"] ["Type1"]
                m2 = mkTestModule "Test" [ImportLine "Data.Either" Nothing $ Set.singleton "Either"] ["Type2"]
                merged = unionModules m1 m2
            Map.size (psImportLines merged) `shouldBe` 2
            length (psTypes merged) `shouldBe` 2

    describe "unionImportLines" $ do
        it "merges maps of import lines" $ do
            let il1 = importsFromList [ImportLine "Data.Maybe" Nothing $ Set.singleton "Maybe"]
                il2 = importsFromList [ImportLine "Data.Either" Nothing $ Set.singleton "Either"]
                merged = unionImportLines il1 il2
            Map.size merged `shouldBe` 2

        it "unions import types for same module" $ do
            let il1 = importsFromList [ImportLine "Data.Maybe" Nothing $ Set.singleton "Maybe"]
                il2 = importsFromList [ImportLine "Data.Maybe" Nothing $ Set.singleton "fromMaybe"]
                merged = unionImportLines il1 il2
            Map.size merged `shouldBe` 1
            let Just il = Map.lookup "Data.Maybe" merged
            importTypes il `shouldBe` Set.fromList ["Maybe", "fromMaybe"]

    describe "unionImportLine" $ do
        it "unions import types" $ do
            let l1 = ImportLine "Data.Maybe" Nothing $ Set.singleton "Maybe"
                l2 = ImportLine "Data.Maybe" Nothing $ Set.singleton "fromMaybe"
                merged = unionImportLine l1 l2
            importTypes merged `shouldBe` Set.fromList ["Maybe", "fromMaybe"]

    describe "typeToImportLines" $ do
        it "creates import for simple type" $ do
            let imports = typeToImportLines psInt
            imports `shouldSatisfy` Map.member "Prim"

        it "creates imports for parameterized type" $ do
            let t = TypeInfo "purescript-maybe" "Data.Maybe" "Maybe" [psInt]
            let imports = typeToImportLines t
            imports `shouldSatisfy` Map.member "Data.Maybe"
            imports `shouldSatisfy` Map.member "Prim"

    describe "isEnum" $ do
        it "returns True when all constructors are Nullary" $ do
            let cs = [ DataConstructor "A" Nullary
                      , DataConstructor "B" Nullary
                      ] :: [DataConstructor 'PureScript]
            isEnum cs `shouldBe` True

        it "returns False when any constructor is non-Nullary" $ do
            let cs = [ DataConstructor "A" Nullary
                      , DataConstructor "B" (Normal $ NE.singleton psInt)
                      ] :: [DataConstructor 'PureScript]
            isEnum cs `shouldBe` False

        it "returns True for empty list" $
            isEnum ([] :: [DataConstructor 'PureScript]) `shouldBe` True

    describe "typeParams" $ do
        it "extracts lowercase type variables" $ do
            let t = TypeInfo "" "" "Maybe" [TypeInfo "" "" "a" []]
                params = typeParams t
            length params `shouldBe` 1
            _typeName (head params) `shouldBe` "a"

        it "skips uppercase type names" $ do
            let t = TypeInfo "" "" "Maybe" [TypeInfo "" "" "Int" []]
            typeParams t `shouldBe` []

        it "extracts from nested types" $ do
            let t = TypeInfo "" "" "Either" [TypeInfo "" "" "a" [], TypeInfo "" "" "b" []]
                params = typeParams t
            length params `shouldBe` 2

        -- Known bug: T.head on empty _typeName crashes (Printer.hs:773)
        it "handles empty typeName without crashing (known bug: partial T.head)" $ do
            let t = TypeInfo "" "" "" [] :: TypeInfo 'PureScript
            typeParams t `shouldBe` []

    describe "flattenTuple" $ do
        it "passes through empty list" $
            flattenTuple [] `shouldBe` ([] :: [TypeInfo 'PureScript])

        it "passes through singleton" $
            flattenTuple [psInt] `shouldBe` [psInt]

        it "flattens nested Tuple in second position" $ do
            let nested = TypeInfo "purescript-tuples" "Data.Tuple" "Tuple"
                            [psString, psInt]
                input = [psInt, nested]
                result = flattenTuple input
            length result `shouldBe` 3

        it "passes through non-tuple types" $
            flattenTuple [psInt, psString] `shouldBe` [psInt, psString]

    describe "hasUnderscore" $ do
        it "returns True for names starting with _" $ do
            let re = RecordEntry "_foo" psInt :: RecordEntry 'PureScript
            hasUnderscore re `shouldBe` True

        it "returns False for names not starting with _" $ do
            let re = RecordEntry "foo" psInt :: RecordEntry 'PureScript
            hasUnderscore re `shouldBe` False

    describe "constructorPattern" $ do
        it "Nullary constructor produces just the name" $ do
            let dc = DataConstructor "Foo" Nullary :: DataConstructor 'PureScript
            renderText (constructorPattern dc) `shouldBe` "Foo"

        it "Normal constructor produces name with variables" $ do
            let dc = DataConstructor "Foo" (Normal $ NE.singleton psInt) :: DataConstructor 'PureScript
            renderText (constructorPattern dc) `shouldSatisfy` T.isPrefixOf "Foo"

        it "Record constructor produces name with record" $ do
            let dc = DataConstructor "Foo" (Record $ NE.singleton $ RecordEntry "x" psInt) :: DataConstructor 'PureScript
            renderText (constructorPattern dc) `shouldSatisfy` T.isPrefixOf "Foo"

    describe "constructor" $ do
        it "Nullary produces unit" $
            renderText (constructor Nullary) `shouldBe` "unit"

        it "Normal with single arg produces 'a'" $
            renderText (constructor (Normal $ NE.singleton psInt)) `shouldBe` "a"

        it "Record produces record expression" $ do
            let args = Record (NE.singleton $ RecordEntry "x" psInt) :: DataConstructorArgs 'PureScript
            renderText (constructor args) `shouldSatisfy` T.isInfixOf "x"

  where
    mkTestModule :: T.Text -> [ImportLine] -> [T.Text] -> Module 'PureScript
    mkTestModule name imports typeNames = PSModule
        { psModuleName = name
        , psImportLines = importsFromList imports
        , psQualifiedImports = Map.empty
        , psTypes = [ SumType (TypeInfo "" name tn []) [DataConstructor tn Nullary] []
                     | tn <- typeNames
                     ]
        }
