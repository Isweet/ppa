module Main (main) where

import Test.Hspec

import qualified PPATest.Lang.While
import qualified PPATest.Lang.While.Internal
import qualified PPATest.Lang.While.Util


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Lang.While"           PPATest.Lang.While.spec
    describe "Lang.While.Internal"  PPATest.Lang.While.Internal.spec
    describe "Lang.While.Util"      PPATest.Lang.While.Util.spec
