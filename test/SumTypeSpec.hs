{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module SumTypeSpec (spec) where

import           Control.Exception (evaluate)
import           Data.Maybe (isJust)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
import           System.Timeout (timeout)
import           Language.PureScript.Bridge.SumType (DataConstructor (..),
                                                     DataConstructorArgs (..),
                                                     ImportLine (..),
                                                     Instance (..),
                                                     RecordEntry (..),
                                                     SumType (..),
                                                     constructorToTypes,
                                                     equal, equal1,
                                                     functor, genericShow,
                                                     getUsedTypes,
                                                     importsFromList,
                                                     instanceToImportLines,
                                                     baselineImports,
                                                     lenses, mkSumType,
                                                     nootype, order, prisms)
import           Language.PureScript.Bridge.TypeInfo (TypeInfo (..), mkTypeInfo,
                                                      Language (Haskell, PureScript))
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty (shouldBe, shouldSatisfy)
import           TestData (Foo, SingleRecord, SomeNewtype, WeekInMonth,
                           SingleValueConstr, SingleProduct)
import           Language.PureScript.Bridge.TypeParameters (A, B)

spec :: Spec
spec = do
    describe "mkSumType" $ do
        it "creates enum type (all Nullary constructors)" $ do
            let SumType ti cs is = mkSumType @WeekInMonth
            _typeName ti `shouldBe` "WeekInMonth"
            length cs `shouldBe` 5
            all (\(DataConstructor _ args) -> args == Nullary) cs `shouldBe` True

        it "includes Generic instance by default" $ do
            let SumType _ _ is = mkSumType @WeekInMonth
            is `shouldSatisfy` elem Generic

        it "creates record newtype (gets Newtype instance)" $ do
            let SumType _ _ is = mkSumType @(SingleRecord A B)
            is `shouldSatisfy` elem Newtype

        it "creates sum type with mixed constructors" $ do
            let SumType ti cs _ = mkSumType @Foo
            _typeName ti `shouldBe` "Foo"
            length cs `shouldBe` 3
            -- Foo has: Foo (Nullary), Bar Int (Normal), FooBar Int Text (Normal)
            let args = map (\(DataConstructor _ a) -> a) cs
            head args `shouldBe` Nullary

        it "creates newtype for single Normal constructor" $ do
            let SumType _ _ is = mkSumType @SomeNewtype
            is `shouldSatisfy` elem Newtype

        it "creates newtype for data type with one value constructor" $ do
            let SumType _ _ is = mkSumType @SingleValueConstr
            is `shouldSatisfy` elem Newtype

        it "does not create Newtype for multi-constructor type" $ do
            let SumType _ _ is = mkSumType @Foo
            is `shouldSatisfy` notElem Newtype

        it "does not create Newtype for product type with two args" $ do
            let SumType _ _ is = mkSumType @SingleProduct
            is `shouldSatisfy` notElem Newtype

    describe "nootype" $ do
        it "returns Just Newtype for single record constructor" $ do
            let cs = [DataConstructor "Foo" (Record $ NE.singleton $ RecordEntry "x" (mkTypeInfo @Int))]
            nootype cs `shouldBe` Just Newtype

        it "returns Just Newtype for single Normal constructor with one arg" $ do
            let cs = [DataConstructor "Foo" (Normal $ NE.singleton $ mkTypeInfo @Int)]
            nootype cs `shouldBe` Just Newtype

        it "returns Nothing for multiple constructors" $ do
            let cs = [ DataConstructor "Foo" Nullary
                      , DataConstructor "Bar" Nullary
                      ]
            nootype cs `shouldBe` Nothing

        it "returns Nothing for single Nullary constructor" $ do
            let cs = [DataConstructor "Foo" Nullary]
            nootype cs `shouldBe` Nothing

        it "returns Nothing for single Normal constructor with multiple args" $ do
            let cs = [DataConstructor "Foo" (Normal $ mkTypeInfo @Int NE.:| [mkTypeInfo @Bool])]
            nootype cs `shouldBe` Nothing

    describe "instance modifiers" $ do
        it "equal adds Eq instance" $ do
            let SumType _ _ is = equal $ mkSumType @Foo
            is `shouldSatisfy` elem Eq

        it "equal is idempotent" $ do
            let SumType _ _ is1 = equal $ mkSumType @Foo
                SumType _ _ is2 = equal . equal $ mkSumType @Foo
            is1 `shouldBe` is2

        it "equal1 adds Eq1 instance" $ do
            let SumType _ _ is = equal1 $ mkSumType @Foo
            is `shouldSatisfy` elem Eq1

        it "order adds Eq and Ord instances" $ do
            let SumType _ _ is = order $ mkSumType @Foo
            is `shouldSatisfy` elem Eq
            is `shouldSatisfy` elem Ord

        it "genericShow adds GenericShow instance" $ do
            let SumType _ _ is = genericShow $ mkSumType @Foo
            is `shouldSatisfy` elem GenericShow

        it "functor adds Functor instance" $ do
            let SumType _ _ is = functor $ mkSumType @Foo
            is `shouldSatisfy` elem Functor

        it "lenses adds Lenses instance" $ do
            let SumType _ _ is = lenses $ mkSumType @Foo
            is `shouldSatisfy` elem Lenses

        it "prisms adds Prisms instance" $ do
            let SumType _ _ is = prisms $ mkSumType @Foo
            is `shouldSatisfy` elem Prisms

    describe "getUsedTypes" $ do
        it "collects types from constructors" $ do
            let st = mkSumType @Foo
                used = getUsedTypes st
            -- Foo has constructors using Int and Text types
            used `shouldSatisfy` (not . Set.null)

        it "collects types from instances" $ do
            let st = mkSumType @WeekInMonth
                used = getUsedTypes st
            -- Should include Generic-related types
            used `shouldSatisfy` (not . Set.null)

        it "includes Generic type info" $ do
            let st = mkSumType @WeekInMonth
                used = getUsedTypes st
                names = Set.map _typeName used
            names `shouldSatisfy` Set.member "class Generic"

    describe "constructorToTypes" $ do
        it "returns empty for Nullary" $ do
            let dc = DataConstructor "Foo" Nullary :: DataConstructor 'Haskell
            constructorToTypes dc `shouldBe` []

        it "returns types for Normal" $ do
            let ti = mkTypeInfo @Int
                dc = DataConstructor "Foo" (Normal $ NE.singleton ti)
            constructorToTypes dc `shouldBe` [ti]

        it "returns record values for Record" $ do
            let ti = mkTypeInfo @Int
                dc = DataConstructor "Foo" (Record $ NE.singleton $ RecordEntry "x" ti)
            constructorToTypes dc `shouldBe` [ti]

    describe "instanceToImportLines" $ do
        it "GenericShow imports genericShow" $ do
            let imports = instanceToImportLines GenericShow
            imports `shouldSatisfy` Map.member "Data.Show.Generic"

        it "Generic produces empty imports" $ do
            let imports = instanceToImportLines Generic
            imports `shouldBe` Map.empty

        it "Newtype imports Data.Lens and friends" $ do
            let imports = instanceToImportLines Newtype
            imports `shouldSatisfy` Map.member "Data.Lens.Iso.Newtype"
            imports `shouldSatisfy` Map.member "Data.Lens.Record"
            imports `shouldSatisfy` Map.member "Type.Proxy"

        it "Enum imports genericPred and genericSucc" $ do
            let imports = instanceToImportLines Enum
            imports `shouldSatisfy` Map.member "Data.Enum.Generic"

        it "Bounded imports genericBottom and genericTop" $ do
            let imports = instanceToImportLines Bounded
            imports `shouldSatisfy` Map.member "Data.Bounded.Generic"

        it "Eq produces empty imports" $
            instanceToImportLines Eq `shouldBe` Map.empty

        it "Ord produces empty imports" $
            instanceToImportLines Ord `shouldBe` Map.empty

        it "Functor produces empty imports" $
            instanceToImportLines Functor `shouldBe` Map.empty

        it "Lenses imports Data.Lens" $ do
            let imports = instanceToImportLines Lenses
            imports `shouldSatisfy` Map.member "Data.Lens"

        -- Known bug: instanceToImportLines Prisms = instanceToImportLines Prisms
        -- (infinite recursion at SumType.hs:470)
        it "Prisms returns import lines (known bug: infinite recursion)" $ do
            result <- timeout 500000 $ evaluate $ Map.size $ instanceToImportLines Prisms
            result `shouldSatisfy` isJust

    describe "importsFromList" $ do
        it "merges imports with same module" $ do
            let imports = importsFromList
                    [ ImportLine "Data.Maybe" Nothing (Set.singleton "Maybe")
                    , ImportLine "Data.Maybe" Nothing (Set.singleton "fromMaybe")
                    ]
            Map.size imports `shouldBe` 1
            let Just il = Map.lookup "Data.Maybe" imports
            importTypes il `shouldBe` Set.fromList ["Maybe", "fromMaybe"]

        it "keeps aliased imports separate" $ do
            let imports = importsFromList
                    [ ImportLine "Data.Map" Nothing (Set.singleton "Map")
                    , ImportLine "Data.Map" (Just "Map") Set.empty
                    ]
            Map.size imports `shouldBe` 2

        it "filters out empty module names" $ do
            let imports = importsFromList
                    [ ImportLine "" Nothing (Set.singleton "Foo")
                    , ImportLine "Data.Maybe" Nothing (Set.singleton "Maybe")
                    ]
            Map.size imports `shouldBe` 1

    describe "baselineImports" $ do
        it "includes Data.Maybe" $
            baselineImports `shouldSatisfy` Map.member "Data.Maybe"

        it "includes Data.Newtype" $
            baselineImports `shouldSatisfy` Map.member "Data.Newtype"

    describe "DataConstructorArgs Semigroup" $ do
        it "Nullary <> b = b" $ do
            let b = Normal $ NE.singleton (mkTypeInfo @Int)
            (Nullary <> b) `shouldBe` b

        it "a <> Nullary = a" $ do
            let a = Normal $ NE.singleton (mkTypeInfo @Int) :: DataConstructorArgs 'Haskell
            (a <> Nullary) `shouldBe` a

        it "Normal <> Normal concatenates" $ do
            let a = Normal $ NE.singleton (mkTypeInfo @Int) :: DataConstructorArgs 'Haskell
                b = Normal $ NE.singleton (mkTypeInfo @Bool)
                Normal result = a <> b
            NE.length result `shouldBe` 2

        it "Record <> Record concatenates" $ do
            let a = Record $ NE.singleton (RecordEntry "x" $ mkTypeInfo @Int) :: DataConstructorArgs 'Haskell
                b = Record $ NE.singleton (RecordEntry "y" $ mkTypeInfo @Bool)
                Record result = a <> b
            NE.length result `shouldBe` 2

        it "Normal <> Record yields Normal (drops labels)" $ do
            let a = Normal $ NE.singleton (mkTypeInfo @Int) :: DataConstructorArgs 'Haskell
                b = Record $ NE.singleton (RecordEntry "y" $ mkTypeInfo @Bool)
                result = a <> b
            isNormal result `shouldBe` True

        it "Record <> Normal yields Normal (drops labels)" $ do
            let a = Record $ NE.singleton (RecordEntry "x" $ mkTypeInfo @Int) :: DataConstructorArgs 'Haskell
                b = Normal $ NE.singleton (mkTypeInfo @Bool)
                result = a <> b
            isNormal result `shouldBe` True

isNormal :: DataConstructorArgs lang -> Bool
isNormal (Normal _) = True
isNormal _          = False
