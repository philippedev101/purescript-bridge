{-# LANGUAGE OverloadedStrings #-}

module PSTypesSpec (spec) where

import           Language.PureScript.Bridge.PSTypes (psBool, psInt, psNumber,
                                                     psString, psUnit, psWord,
                                                     psWord16, psWord32,
                                                     psWord64, psWord8)
import           Language.PureScript.Bridge.TypeInfo (TypeInfo (..))
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty (shouldBe)

spec :: Spec
spec = do
    describe "psBool" $ do
        it "has correct module and name" $ do
            _typeModule psBool `shouldBe` "Prim"
            _typeName psBool `shouldBe` "Boolean"
            _typePackage psBool `shouldBe` ""
            _typeParameters psBool `shouldBe` []

    describe "psInt" $ do
        it "has correct module and name" $ do
            _typeModule psInt `shouldBe` "Prim"
            _typeName psInt `shouldBe` "Int"
            _typePackage psInt `shouldBe` ""
            _typeParameters psInt `shouldBe` []

    describe "psNumber" $ do
        it "has correct module and name" $ do
            _typeModule psNumber `shouldBe` "Prim"
            _typeName psNumber `shouldBe` "Number"
            _typePackage psNumber `shouldBe` ""
            _typeParameters psNumber `shouldBe` []

    describe "psString" $ do
        it "has correct module and name" $ do
            _typeModule psString `shouldBe` "Prim"
            _typeName psString `shouldBe` "String"
            _typePackage psString `shouldBe` ""
            _typeParameters psString `shouldBe` []

    describe "psUnit" $ do
        it "has correct package, module, and name" $ do
            _typePackage psUnit `shouldBe` "purescript-prelude"
            _typeModule psUnit `shouldBe` "Prelude"
            _typeName psUnit `shouldBe` "Unit"
            _typeParameters psUnit `shouldBe` []

    describe "psWord" $ do
        it "has correct package, module, and name" $ do
            _typePackage psWord `shouldBe` "purescript-word"
            _typeModule psWord `shouldBe` "Data.Word"
            _typeName psWord `shouldBe` "Word"
            _typeParameters psWord `shouldBe` []

    describe "psWord8" $ do
        it "has correct name" $ do
            _typePackage psWord8 `shouldBe` "purescript-word"
            _typeModule psWord8 `shouldBe` "Data.Word"
            _typeName psWord8 `shouldBe` "Word8"

    describe "psWord16" $ do
        it "has correct name" $ do
            _typePackage psWord16 `shouldBe` "purescript-word"
            _typeModule psWord16 `shouldBe` "Data.Word"
            _typeName psWord16 `shouldBe` "Word16"

    describe "psWord32" $ do
        it "has correct name" $ do
            _typePackage psWord32 `shouldBe` "purescript-word"
            _typeModule psWord32 `shouldBe` "Data.Word"
            _typeName psWord32 `shouldBe` "Word32"

    describe "psWord64" $ do
        it "has correct name" $ do
            _typePackage psWord64 `shouldBe` "purescript-word"
            _typeModule psWord64 `shouldBe` "Data.Word"
            _typeName psWord64 `shouldBe` "Word64"
