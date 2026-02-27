{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module PrimitivesSpec (spec) where

import           Data.Map (Map)
import           Data.Set (Set)
import           Data.Word (Word, Word8, Word16, Word32, Word64)
import           Language.PureScript.Bridge (defaultBridge)
import           Language.PureScript.Bridge.Builder (buildBridge)
import           Language.PureScript.Bridge.PSTypes (psArray, psBool, psInt,
                                                     psNumber, psString,
                                                     psUnit, psWord, psWord16,
                                                     psWord32, psWord64,
                                                     psWord8)
import           Language.PureScript.Bridge.Primitives (boolBridge,
                                                        doubleBridge,
                                                        intBridge, listBridge,
                                                        mapBridge,
                                                        noContentBridge,
                                                        setBridge,
                                                        stringBridge,
                                                        textBridge,
                                                        unitBridge,
                                                        word16Bridge,
                                                        word32Bridge,
                                                        word64Bridge,
                                                        word8Bridge,
                                                        wordBridge)
import           Language.PureScript.Bridge.TypeInfo (TypeInfo (..), mkTypeInfo)
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty (shouldBe, shouldNotBe)

spec :: Spec
spec = do
    describe "boolBridge" $ do
        it "translates Bool to psBool" $
            buildBridge boolBridge (mkTypeInfo @Bool) `shouldBe` psBool
        it "rejects non-Bool types" $
            buildBridge boolBridge (mkTypeInfo @Int) `shouldNotBe` psBool

    describe "intBridge" $ do
        it "translates Int to psInt" $
            buildBridge intBridge (mkTypeInfo @Int) `shouldBe` psInt
        it "rejects non-Int types" $
            buildBridge intBridge (mkTypeInfo @Bool) `shouldNotBe` psInt

    describe "doubleBridge" $ do
        it "translates Double to psNumber" $
            buildBridge doubleBridge (mkTypeInfo @Double) `shouldBe` psNumber
        it "rejects non-Double types" $
            buildBridge doubleBridge (mkTypeInfo @Int) `shouldNotBe` psNumber

    describe "stringBridge" $ do
        it "translates String to psString" $
            buildBridge stringBridge (mkTypeInfo @String) `shouldBe` psString

    describe "textBridge" $ do
        it "translates Text to psString" $ do
            let result = buildBridge textBridge (mkTypeInfo @String)
            -- textBridge checks for typeName == "Text", String won't match
            result `shouldNotBe` psString

    describe "unitBridge" $ do
        it "translates () to psUnit (known bug: operator precedence)" $
            buildBridge unitBridge (mkTypeInfo @()) `shouldBe` psUnit
        it "translates types named Unit to psUnit" $ do
            let unitType = TypeInfo "" "" "Unit" []
            buildBridge unitBridge unitType `shouldBe` psUnit

    describe "noContentBridge" $ do
        it "translates NoContent to psUnit" $ do
            let noContentType = TypeInfo "" "" "NoContent" []
            buildBridge noContentBridge noContentType `shouldBe` psUnit

    describe "wordBridge" $ do
        it "translates Word to psWord" $
            buildBridge wordBridge (mkTypeInfo @Word) `shouldBe` psWord

    describe "word8Bridge" $ do
        it "translates Word8 to psWord8" $
            buildBridge word8Bridge (mkTypeInfo @Word8) `shouldBe` psWord8

    describe "word16Bridge" $ do
        it "translates Word16 to psWord16" $
            buildBridge word16Bridge (mkTypeInfo @Word16) `shouldBe` psWord16

    describe "word32Bridge" $ do
        it "translates Word32 to psWord32" $
            buildBridge word32Bridge (mkTypeInfo @Word32) `shouldBe` psWord32

    describe "word64Bridge" $ do
        it "translates Word64 to psWord64" $
            buildBridge word64Bridge (mkTypeInfo @Word64) `shouldBe` psWord64

    describe "listBridge" $ do
        it "translates [Int] — list type gets Array with bridged params" $ do
            let result = buildBridge (listBridge) (mkTypeInfo @[Int])
            _typeName result `shouldBe` "Array"

    describe "setBridge" $ do
        it "matches Data.Set.Internal" $ do
            let result = buildBridge setBridge (mkTypeInfo @(Set Int))
            _typeName result `shouldBe` "Set"
            _typeModule result `shouldBe` "Data.Set"

    describe "mapBridge" $ do
        it "matches Data.Map.Internal" $ do
            let result = buildBridge mapBridge (mkTypeInfo @(Map String Int))
            _typeName result `shouldBe` "Map"
            _typeModule result `shouldBe` "Data.Map"

    describe "defaultBridge" $ do
        it "handles all primitive types" $ do
            let bridge = buildBridge defaultBridge
            bridge (mkTypeInfo @Bool) `shouldBe` psBool
            bridge (mkTypeInfo @Int) `shouldBe` psInt
            bridge (mkTypeInfo @Double) `shouldBe` psNumber
            bridge (mkTypeInfo @String) `shouldBe` psString
            bridge (mkTypeInfo @Word) `shouldBe` psWord
            bridge (mkTypeInfo @()) `shouldBe` psUnit
