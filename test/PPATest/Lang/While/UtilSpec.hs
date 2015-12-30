module PPATest.Lang.While.UtilSpec (spec) where

import Prelude hiding (init)

import Test.Hspec

import PPATest.Common

import qualified Data.Set as Set

import PPA.Lang.While.Util
import PPA.Lang.While.Syntax

spec :: Spec
spec = do
    describe "init" $ do
        it "-- ifBasic" $ do
            (init $ ifBasicS) `shouldBe` 1
        it "-- ifTwice" $ do
            (init $ ifTwiceS) `shouldBe` 1
        it "-- power" $ do
            (init $ powerS) `shouldBe` 1
        it "-- factorial" $ do
            (init $ factorialS) `shouldBe` 1
        it "-- available" $ do
            (init $ availableS) `shouldBe` 1
    describe "final" $ do
        it "-- ifBasic" $ do
            (final $ ifBasicS) `shouldBe` Set.fromList [2, 3]
        it "-- ifTwice" $ do
            (final $ ifTwiceS) `shouldBe` Set.fromList [5, 6]
        it "-- power" $ do
            (final $ powerS) `shouldBe` Set.fromList [2]
        it "-- factorial" $ do
            (final $ factorialS) `shouldBe` Set.fromList [6]
        it "-- available" $ do
            (final $ availableS) `shouldBe` Set.fromList [3]
    describe "flow" $ do
        it "-- ifBasic" $
            (flow $ ifBasicS) `shouldBe` Set.fromList [(1,2), (1,3)]
        it "-- ifTwice" $
            (flow $ ifTwiceS) `shouldBe` Set.fromList [(1,2), (1,3), (2,4), (3,4), (4,5), (4,6)]
        it "-- power" $
            (flow $ powerS) `shouldBe` Set.fromList [(1,2), (2,3), (3,4), (4,2)]
        it "-- factorial" $
            (flow $ factorialS) `shouldBe` Set.fromList [(1,2), (2,3), (3,4), (3,6), (4,5), (5,3)]
    describe "free variables (fv)" $
        it "-- factorial" $
            (fv $ factorialS) `shouldBe` Set.fromList ["x", "y", "z"]

                where
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
