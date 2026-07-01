{-# LANGUAGE QuasiQuotes #-}
module Data.TextTemplate.QQInternalSpec (spec) where

import Test.Hspec
import Data.TextTemplate.TemplateInternal
import Data.TextTemplate.QQInternal

import Data.IntMap qualified as M
import Data.Text (Text)

spec :: Spec 
spec = do
    describe "quasi-quoting tests:" $ do
        it "chunk test 1" $ do
            (fst testChunk1) `shouldBe` (snd testChunk1)
        it "chunk test 2" $ do
            (fst testChunk2) `shouldBe` (snd testChunk2)
        it "chunk test 3" $ do
            (fst testChunk3) `shouldBe` (snd testChunk3)
        it "hole test 1" $ do
            (fst testHole1) `shouldBe` (snd testHole1)
        it "hole test 2" $ do
            (fst testHole2) `shouldBe` (snd testHole2)
        it "hole test 3" $ do
            (fst testHole3) `shouldBe` (snd testHole3)
        it "hole test 4" $ do
            (fst testHole4) `shouldBe` (snd testHole4)
        it "hole test 5" $ do
            (fst testHole5) `shouldBe` (snd testHole5)          
        it "filled hole test 1" $ 
            (fst testFilledHole1) `shouldBe` (snd testFilledHole1)
        it "filled hole test 2" $ 
            (fst (testFilledHole2 3)) `shouldBe` (snd (testFilledHole2 3))
        it "filled hole test 3" $ 
            (fst testFilledHole3) `shouldBe` (snd testFilledHole3)

testChunk1 :: (Template (),Template ())
testChunk1 = ([template|this is a chunk|],chunk "this is a chunk")

testChunk2 :: (Template (),Template ())
testChunk2 = ([template| |],Template (IChunk " ") emptyHoleProps)

testChunk3 :: (Template (),Template ())
testChunk3 = ([template|😩|],Template (IChunk "😩") emptyHoleProps)

testHole1 :: (Template (),Template ())
testHole1 = ([template|$1{}$2{}$3{}|],Template (ICompose "" 1 (ICompose "" 2 (ICompose "" 3 (IChunk "")))) ([1,2,3],M.empty))

testHole2 :: (Template (),Template ())
testHole2 = ([template|$1{}$2{}-$3{}|],Template (ICompose "" 1 (ICompose "" 2 (ICompose "-" 3 (IChunk "")))) ([1,2,3],M.empty))

testHole3 :: (Template (),Template ())
testHole3 = ([template|this $1{} and $2{} is $1{}|],Template (ICompose "this " 1 (ICompose " and " 2 (ICompose " is " 1 (IChunk "")))) ([1,2],M.empty))

testHole4 :: (Template (),Template ())
testHole4 = ([template|Hi $1{}!|], Template (ICompose "Hi " 1 (IChunk "!")) ([1],M.empty))

testHole5 :: (Template (),Template ())
testHole5 = ([template|Hi ❤️, $1{} ‼|], Template (ICompose "Hi ❤️, " 1 (IChunk " ‼")) ([1],M.empty))

testFilledHole1 :: (Template Text,Template Text)
testFilledHole1 = ([template|before-$1{"example filling"}-and-after|], Template (ICompose "before-" 1 (IChunk "-and-after")) ([],M.fromList [(1,"example filling")]))

testFilledHole2 :: Int -> (Template Int,Template Int)
testFilledHole2 x = ([template|before-$1{x}-and-after|], Template (ICompose "before-" 1 (IChunk "-and-after")) ([],M.fromList [(1,x)]))

testFilledHole3 :: (Template String,Template String)
testFilledHole3 = ([template|before-$1{"\\ $ ( ) { \" '' }"}-and-after|], Template (ICompose "before-" 1 (IChunk "-and-after")) ([],M.fromList [(1,"\\ $ ( ) { \" '' }")]))

