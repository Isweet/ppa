module PPATest.Lang.While.SyntaxSpec (spec) where

import Test.Hspec

import PPATest.Common

import PPA.Lang.While.Syntax

spec :: Spec
spec = do
    describe "read . show" $ do
        it "-- ifBasic" $
            (read . show) ifBasic `shouldBe` ifBasic
        it "-- ifTwice" $
            (read . show) ifTwice `shouldBe` ifTwice
        it "-- power" $
            (read . show) power `shouldBe` power
        it "-- factorial" $
            (read . show) factorial `shouldBe` factorial
        it "-- available" $
            (read . show) available `shouldBe` available
