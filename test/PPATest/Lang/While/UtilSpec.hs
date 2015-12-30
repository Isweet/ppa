module PPATest.Lang.While.UtilSpec (spec) where

import Test.Hspec

import PPATest.Common

import qualified Data.Set as Set

import PPA.Lang.While.Util

spec :: Spec
spec = do
    describe "flow" $ do
        it "-- factorial" $
            (flow $ read factorial) `shouldBe` Set.fromList [(1,2), (2,3), (3,4), (3,6), (4,5), (5,3)]
        it "-- power" $
            (flow $ read power) `shouldBe` Set.fromList [(1,2), (2,3), (3,4), (4,2)]
