{-|
Module      : JSONSpec
Description : Tests for JSON Templates
Copyright   : (c) Harley Eades, 2026
              (c) WKB3, 2026
Maintainer  : harley.eades@gmail.com

-}
{-# LANGUAGE QuasiQuotes #-}
module  Data.StringTemplate.JSONSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import Data.Text qualified as DT

import Data.StringTemplate.JSON(jsonTemplate)
import Data.StringTemplate.TemplateInternal
import Test.QuickCheck (property, Property)

testArray1 :: (Text,Text)
testArray1 = (DT.show [jsonTemplate|["1"]|],"[\"1\"]")

testArray2 :: (Text,Text)
testArray2 = (DT.show [jsonTemplate|["1",'$1{}',3]|],"[\"1\",$1{},3]")

testBool1 :: (Text,Text)
testBool1 = (DT.show [jsonTemplate|true|],"true")

testBool0 :: (Text,Text)
testBool0 = (DT.show [jsonTemplate|false|],"false")

--prop_number :: Double -> Property
--prop_number n = property $ undefined -- test numVParser

spec :: Spec
spec = do
    describe "unit tests:" $ do
        describe "literals" $ do
            describe "booleans:" $ do
                it "boolean true" $ do
                    (fst testBool1) `shouldBe` (snd testBool1)
                it "boolean false" $ do
                    (fst testBool0) `shouldBe` (snd testBool0)
        describe "arrays:" $ do
            it "singleton-array" $ do
                (fst testArray1) `shouldBe` (snd testArray1)
            it "mixed-type-array-with-1-hole" $ do
                (fst testArray2) `shouldBe` (snd testArray2)
