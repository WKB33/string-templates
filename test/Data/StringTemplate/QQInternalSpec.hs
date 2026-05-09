{-# LANGUAGE QuasiQuotes #-}
module Data.StringTemplate.QQInternalSpec (spec) where

import Test.Hspec

import Data.StringTemplate

testChunkOnly :: Template
testChunkOnly = [template|this is a chunk|]

test1Hole :: Template
test1Hole = [template|Hi ${1}!|]

testJSON1 :: Template
testJSON1 = [template|
{
    "forename": "${1}", 
    "surname": "${2}"
}
|]

testJSON2 :: Template
testJSON2 = [template|{"forename":"${1}","surname":"${2}"}|]

spec :: Spec 
spec = undefined
