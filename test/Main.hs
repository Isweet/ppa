module Main (main) where

import Test.Hspec

import qualified PPATest.Lang.While.SyntaxSpec
import qualified PPATest.Lang.While.UtilSpec

import qualified PPATest.Lang.While.Internal.SyntaxSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Lang.While.SyntaxSpec"              PPATest.Lang.While.SyntaxSpec.spec
  describe "Lang.While.UtilSpec"                PPATest.Lang.While.UtilSpec.spec
  describe "Lang.While.Internal.SyntaxSpec"     PPATest.Lang.While.Internal.SyntaxSpec.spec
