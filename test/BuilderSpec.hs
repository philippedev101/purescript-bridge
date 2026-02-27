{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module BuilderSpec (spec) where

import           Control.Exception (evaluate)
import           Language.PureScript.Bridge.Builder (BridgePart, buildBridge,
                                                     buildBridgeWithCustomFixUp,
                                                     clearPackageFixUp,
                                                     doCheck, errorFixUp,
                                                     psTypeParameters, (^==),
                                                     (<|>))
import           Language.PureScript.Bridge.PSTypes (psInt, psNumber, psString)
import           Language.PureScript.Bridge.Primitives (intBridge)
import           Language.PureScript.Bridge.TypeInfo (TypeInfo (..), mkTypeInfo,
                                                      typeModule, typeName)
import           Test.Hspec (Spec, anyException, describe, it, shouldThrow)
import           Test.Hspec.Expectations.Pretty (shouldBe, shouldNotBe)

spec :: Spec
spec = do
    describe "buildBridge" $ do
        it "with matching bridge returns correct PSType" $ do
            let bridge :: BridgePart
                bridge = typeName ^== "Int" >> return psInt
                result = buildBridge bridge (mkTypeInfo @Int)
            result `shouldBe` psInt

        it "with no matching bridge falls back to clearPackageFixUp" $ do
            let bridge :: BridgePart
                bridge = typeName ^== "NONEXISTENT" >> return psInt
                result = buildBridge bridge (mkTypeInfo @Int)
            -- clearPackageFixUp copies module and name, clears package
            _typeName result `shouldBe` "Int"
            _typePackage result `shouldBe` ""

    describe "buildBridgeWithCustomFixUp" $ do
        it "errorFixUp errors on unknown type" $ do
            let bridge :: BridgePart
                bridge = typeName ^== "NONEXISTENT" >> return psInt
                result = buildBridgeWithCustomFixUp errorFixUp bridge (mkTypeInfo @Int)
            evaluate (seq result ()) `shouldThrow` anyException

    describe "(^==)" $ do
        it "matches when equal" $ do
            let bridge :: BridgePart
                bridge = typeName ^== "Int" >> return psString
                result = buildBridge bridge (mkTypeInfo @Int)
            result `shouldBe` psString

        it "does not match when unequal (falls back)" $ do
            let bridge :: BridgePart
                bridge = typeName ^== "Bool" >> return psString
                result = buildBridge bridge (mkTypeInfo @Int)
            -- Falls back to clearPackageFixUp, not psString
            result `shouldNotBe` psString

        it "can check typeModule" $ do
            let bridge :: BridgePart
                bridge = do
                    typeName ^== "Int"
                    typeModule ^== _typeModule (mkTypeInfo @Int)
                    return psNumber
                result = buildBridge bridge (mkTypeInfo @Int)
            result `shouldBe` psNumber

    describe "doCheck" $ do
        it "passes with true predicate" $ do
            let bridge :: BridgePart
                bridge = doCheck typeName (== "Int") >> return psString
                result = buildBridge bridge (mkTypeInfo @Int)
            result `shouldBe` psString

        it "fails with false predicate (falls back)" $ do
            let bridge :: BridgePart
                bridge = doCheck typeName (== "Bool") >> return psString
                result = buildBridge bridge (mkTypeInfo @Int)
            result `shouldNotBe` psString

    describe "Alternative (<|>)" $ do
        it "first match wins" $ do
            let b1 :: BridgePart
                b1 = typeName ^== "Int" >> return psInt
                b2 :: BridgePart
                b2 = typeName ^== "Int" >> return psString
                result = buildBridge (b1 <|> b2) (mkTypeInfo @Int)
            result `shouldBe` psInt

        it "falls back to second when first fails" $ do
            let b1 :: BridgePart
                b1 = typeName ^== "Bool" >> return psInt
                b2 :: BridgePart
                b2 = typeName ^== "Int" >> return psString
                result = buildBridge (b1 <|> b2) (mkTypeInfo @Int)
            result `shouldBe` psString

        it "falls back to fixup when both fail" $ do
            let b1 :: BridgePart
                b1 = typeName ^== "Bool" >> return psInt
                b2 :: BridgePart
                b2 = typeName ^== "Bool" >> return psString
                result = buildBridge (b1 <|> b2) (mkTypeInfo @Int)
            _typePackage result `shouldBe` ""
            _typeName result `shouldBe` "Int"

    describe "fixTypeParameters (via buildBridge)" $ do
        it "lowercases and strips module for TypeParameters types" $ do
            let bridge :: BridgePart
                bridge = return $ TypeInfo "some-pkg" "Foo.TypeParameters" "A" []
                result = buildBridge bridge (mkTypeInfo @Int)
            result `shouldBe` TypeInfo "" "" "a" []

        it "strips trailing '1' from TypeParameters names" $ do
            let bridge :: BridgePart
                bridge = return $ TypeInfo "some-pkg" "Foo.TypeParameters" "A1" []
                result = buildBridge bridge (mkTypeInfo @Int)
            result `shouldBe` TypeInfo "" "" "a" []

        it "leaves non-TypeParameters types unchanged" $ do
            let bridge :: BridgePart
                bridge = return $ TypeInfo "my-pkg" "My.Module" "MyType" []
                result = buildBridge bridge (mkTypeInfo @Int)
            result `shouldBe` TypeInfo "my-pkg" "My.Module" "MyType" []

    describe "psTypeParameters (via buildBridge)" $ do
        it "bridges type parameters using the full bridge" $ do
            let myBridge :: BridgePart
                myBridge = do
                    typeName ^== "Maybe"
                    params <- psTypeParameters
                    return $ TypeInfo "my-pkg" "My.Module" "MyMaybe" params
                combined = myBridge <|> intBridge
                result = buildBridge combined (mkTypeInfo @(Maybe Int))
            result `shouldBe` TypeInfo "my-pkg" "My.Module" "MyMaybe" [psInt]
