{-# LANGUAGE QuasiQuotes #-}
module Data.StringTemplate.QQInternalSpec (spec) where

import Test.Hspec

import Data.StringTemplate

testChunkOnly :: Template
testChunkOnly = [template|this is a chunk|]

test1Hole :: Template
test1Hole = [template|Hi ${1}!|]

spec :: Spec 
spec = undefined
