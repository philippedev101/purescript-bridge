{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module TupleSpec (spec) where

import           Language.PureScript.Bridge.Tuple (TupleParserState (..), isTuple, step)
import           Language.PureScript.Bridge.TypeInfo (TypeInfo (..), mkTypeInfo)
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.Expectations.Pretty (shouldBe)

spec :: Spec
spec = do
    describe "step (state machine transitions)" $ do
        -- Start state transitions
        it "Start + '(' -> OpenFound" $
            step Start '(' `shouldBe` OpenFound
        it "Start + any other char -> NoTuple" $
            step Start 'a' `shouldBe` NoTuple
        it "Start + ',' -> NoTuple" $
            step Start ',' `shouldBe` NoTuple

        -- OpenFound state transitions
        it "OpenFound + ',' -> ColonFound" $
            step OpenFound ',' `shouldBe` ColonFound
        it "OpenFound + ')' -> NoTuple" $
            step OpenFound ')' `shouldBe` NoTuple
        it "OpenFound + 'x' -> NoTuple" $
            step OpenFound 'x' `shouldBe` NoTuple

        -- ColonFound state transitions
        it "ColonFound + ',' -> ColonFound" $
            step ColonFound ',' `shouldBe` ColonFound
        it "ColonFound + ')' -> Tuple" $
            step ColonFound ')' `shouldBe` Tuple
        it "ColonFound + 'x' -> NoTuple" $
            step ColonFound 'x' `shouldBe` NoTuple

        -- Tuple state (absorbing)
        it "Tuple + any char -> NoTuple" $
            step Tuple 'x' `shouldBe` NoTuple
        it "Tuple + ')' -> NoTuple" $
            step Tuple ')' `shouldBe` NoTuple

        -- NoTuple state (absorbing)
        it "NoTuple + any char -> NoTuple" $
            step NoTuple 'x' `shouldBe` NoTuple
        it "NoTuple + '(' -> NoTuple" $
            step NoTuple '(' `shouldBe` NoTuple

    describe "isTuple" $ do
        it "recognizes \"(,)\" as a tuple" $
            isTuple (mkTI "(,)") `shouldBe` True

        it "recognizes \"(,,)\" as a tuple" $
            isTuple (mkTI "(,,)") `shouldBe` True

        it "recognizes \"(,,,)\" as a tuple" $
            isTuple (mkTI "(,,,)") `shouldBe` True

        it "rejects \"Int\"" $
            isTuple (mkTI "Int") `shouldBe` False

        it "rejects \"Maybe\"" $
            isTuple (mkTI "Maybe") `shouldBe` False

        it "rejects empty string" $
            isTuple (mkTI "") `shouldBe` False

        it "rejects \"(\"" $
            isTuple (mkTI "(") `shouldBe` False

        it "rejects \"(,\"" $
            isTuple (mkTI "(,") `shouldBe` False

        it "rejects \"()\" (unit, not a tuple)" $
            isTuple (mkTI "()") `shouldBe` False

        it "rejects \"(,,\" (no closing paren)" $
            isTuple (mkTI "(,,") `shouldBe` False

  where
    mkTI name = TypeInfo "" "" name []
