module PPATest.Lang.While (spec) where

import Test.Hspec

import PPATest.Common

import PPA.Lang.While

spec :: Spec
spec = do
    describe "read . show" $ do
        it "-- ifBasic" $
            rs ifBasicS `shouldBe` ifBasicS
        it "-- ifTwice" $
            rs ifTwiceS `shouldBe` ifTwiceS
        it "-- power" $
            rs powerS `shouldBe` powerS
        it "-- factorial" $
            rs factorialS `shouldBe` factorialS
        it "-- available" $
            rs availableS `shouldBe` availableS
                where

                    rs :: Stmt -> Stmt
                    rs = (read . show)

                    ifBasicS :: Stmt
                    ifBasicS = read ifBasic

                    ifTwiceS :: Stmt
                    ifTwiceS = read ifTwice

                    powerS :: Stmt
                    powerS = read power

                    factorialS :: Stmt
                    factorialS = read factorial

                    availableS :: Stmt
                    availableS = read available

