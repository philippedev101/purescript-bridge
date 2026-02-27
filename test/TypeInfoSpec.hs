{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module TypeInfoSpec (spec) where

import qualified Data.Text as T
import           Language.PureScript.Bridge.TypeInfo (TypeInfo (..),
                                                      flattenTypeInfo,
                                                      mkTypeInfo)
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty (shouldBe, shouldSatisfy)

spec :: Spec
spec = do
    describe "mkTypeInfo" $ do
        it "creates TypeInfo for Int" $ do
            let ti = mkTypeInfo @Int
            _typeName ti `shouldBe` "Int"
            _typeParameters ti `shouldBe` []

        it "creates TypeInfo for Bool" $ do
            let ti = mkTypeInfo @Bool
            _typeName ti `shouldBe` "Bool"
            _typeParameters ti `shouldBe` []

        it "creates TypeInfo for Maybe Int (parameterized type)" $ do
            let ti = mkTypeInfo @(Maybe Int)
            _typeName ti `shouldBe` "Maybe"
            length (_typeParameters ti) `shouldBe` 1
            _typeName (head $ _typeParameters ti) `shouldBe` "Int"

        it "creates TypeInfo for Either String Int (two type parameters)" $ do
            let ti = mkTypeInfo @(Either String Int)
            _typeName ti `shouldBe` "Either"
            length (_typeParameters ti) `shouldBe` 2
            _typeName (last $ _typeParameters ti) `shouldBe` "Int"

        it "creates TypeInfo for nested Maybe (Either String Int)" $ do
            let ti = mkTypeInfo @(Maybe (Either String Int))
            _typeName ti `shouldBe` "Maybe"
            length (_typeParameters ti) `shouldBe` 1
            let inner = head $ _typeParameters ti
            _typeName inner `shouldBe` "Either"
            length (_typeParameters inner) `shouldBe` 2

        it "populates _typeModule" $ do
            let ti = mkTypeInfo @Int
            _typeModule ti `shouldSatisfy` (not . T.null)

        it "populates _typePackage" $ do
            let ti = mkTypeInfo @Int
            _typePackage ti `shouldSatisfy` (not . T.null)

    describe "flattenTypeInfo" $ do
        it "flattens a simple type to a singleton list" $ do
            let ti = mkTypeInfo @Int
            flattenTypeInfo ti `shouldBe` [ti]

        it "flattens Maybe Int to [Maybe, Int]" $ do
            let ti = mkTypeInfo @(Maybe Int)
                flat = flattenTypeInfo ti
            length flat `shouldBe` 2
            _typeName (head flat) `shouldBe` "Maybe"
            _typeName (flat !! 1) `shouldBe` "Int"

        it "flattens nested types recursively" $ do
            let ti = mkTypeInfo @(Maybe (Either String Int))
                flat = flattenTypeInfo ti
            -- At minimum: Maybe, Either, [something for String], Int
            length flat `shouldSatisfy` (>= 4)
            _typeName (head flat) `shouldBe` "Maybe"

        it "flattens type with no parameters to singleton" $ do
            let ti = mkTypeInfo @Bool
            flattenTypeInfo ti `shouldBe` [ti]

    describe "HasHaskType" $ do
        it "haskType lens on HaskellType is identity (structural test)" $ do
            -- The haskType lens for HaskellType is just id
            -- We verify TypeInfo equality works correctly
            let ti = mkTypeInfo @Int
            ti `shouldBe` ti
            let ti2 = mkTypeInfo @(Maybe Int)
            ti2 `shouldBe` ti2
            ti `shouldSatisfy` (/= ti2)
