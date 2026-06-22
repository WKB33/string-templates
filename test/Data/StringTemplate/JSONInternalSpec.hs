{-|
Module      : JSONSpec
Description : Tests for JSON Templates
Copyright   : (c) Harley Eades, 2026
              (c) WKB3, 2026
Maintainer  : harley.eades@gmail.com

-}
{-# LANGUAGE QuasiQuotes #-}
module  Data.StringTemplate.JSONInternalSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (property, Property)
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.Natural ()
import Data.Text (Text)
import Data.Text qualified as DT
import Data.Char (isSpace, isControl, isPrint)

import Data.StringTemplate.JSONInternal
import Data.StringTemplate.TemplateInternal
import Data.Maybe (isJust)

spec :: Spec
spec = do    
    describe "literals" $ do
        test_case "empty"                                    test_empty
        test_case "boolean-true"                             test_bool1
        test_case "boolean-false"                            test_bool0
        test_case "parsing-null"                             test_null        
        prop "parsing-numbers"                               prop_numberParser
        test_case "parsing-escaped-quotes"                   test_quotedString
        prop "parsing-strings"                               prop_stringParser
    describe "arrays:" $ do
        test_case "singleton-array"                          test_array1
        test_case "mixed-type-array-with-1-hole"             test_array2
        test_case "empty-array"                              test_array3
        test_case "trailing-comma"                           test_array4
        test_case "no-commas"                                test_array5
        test_case "heterogenous-array"                       test_array6
        test_case "various-whitespace"                       test_array7
        test_case "deeply-nested-array"                      test_array8
        test_case "mismatched-brackets"                      test_array9
        test_case "missing-closing-bracket"                  test_array10
        test_case "unicode-elements"                         test_array11
    describe "objects:" $ do
        test_case "empty-object"                             test_object1
        test_case "no-comma-after-label"                     test_object2
        test_case "missing-closing-brace"                    test_object3
        test_case "singleton-object"                         test_object4
        test_case "empty-field-label"                        test_object5
        test_case "duplicate-keys"                           test_object6
        test_case "various-whitespace"                       test_object7
        test_case "unicode-in-fields"                        test_object8
        test_case "unicode-in-fields-values-surrogate-pairs" test_object9        
        json <- runIO $ readFile "test/example-data/github-public-repos.json"
        test_case "large-object-github-public-repos"         (test_object10 json)        

-- | The empty string is an error.
test_empty :: UnitTest (Maybe Value)
test_empty = UnitTest {
         test_output = testParser valueParser ""
        ,test_result = Nothing
    }

-- * Literals
-- ** Booleans
test_bool1 :: UnitTest Text
test_bool1 = UnitTest {
         test_output = DT.show [jsonTemplate|true|]
        ,test_result = "true"
    }

test_bool0 :: UnitTest Text
test_bool0 = UnitTest {
         test_output = DT.show [jsonTemplate|false|]
        ,test_result = "false"
    }

-- ** Null
test_null :: UnitTest Text
test_null = UnitTest {
         test_output = DT.show [jsonTemplate|null|]
        ,test_result = "null"
    }

-- ** Strings
isJSONStr :: Text -> Bool
isJSONStr DT.Empty                  = True
isJSONStr ('\\' DT.:< x DT.:< rest) = isEscapeChar x       && isJSONStr rest
isJSONStr (x DT.:< rest)            = not (isEscapeChar x || isSpace x || isControl x) && isPrint x && isJSONStr rest

testParser :: Parser a -> Text -> Maybe a
testParser p = eitherToMaybe . parse p

test_quotedString :: UnitTest (Maybe Value)
test_quotedString = UnitTest {
         test_output = testParser strVParser "\"\\\"New\\\"\""
        ,test_result = Just . StrV $ "\"New\""
    }

prop_stringParser :: DT.Text -> Property
prop_stringParser s = property $ if isJSONStr s then _parse s == ans s else True
    where
        _parse = testParser strVParser . ("\""<>) . (<>"\"")
        ans = Just . StrV

-- ** Numbers
prop_numberParser :: Double -> Property
prop_numberParser n = property $ _parse n == ans n
    where
        _parse = testParser numVParser . DT.show
        ans = Just . NumV

-- * Arrays
test_array1 :: UnitTest Text
test_array1 = UnitTest {
         test_output = (DT.show [jsonTemplate|["1"]|]) 
        ,test_result = "[\"1\"]"
    }

test_array2 :: UnitTest Text
test_array2 = UnitTest {
         test_output = DT.show [jsonTemplate|["1",'$1{}',3]|]
        ,test_result = "[\"1\",$1{},3]"
    }

-- | The type of a unit test corresponds to a pair of an output value and an
-- expected result.
data UnitTest a = UnitTest {
     test_output :: a -- ^ Output of a computation
    ,test_result :: a -- ^ Expected result of the test
}

test_array3 :: UnitTest (Maybe Value)
test_array3 = UnitTest {
         test_output = testParser arrayVParser $ "[]"
        ,test_result = Just . ArrayV $ []
    } 

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right x) = Just x
eitherToMaybe (Left _)  = Nothing

test_array4 :: UnitTest (Maybe Value)
test_array4 = UnitTest {
         test_output = testParser arrayVParser $ "[1,]"
        ,test_result = Nothing
    }

test_array5 :: UnitTest (Maybe Value)
test_array5 = UnitTest {
         test_output = testParser arrayVParser $ "[1 2 3 4 5]"
        ,test_result = Nothing
    }

test_array6 :: UnitTest (Maybe Value)
test_array6 = UnitTest {
         test_output = testParser arrayVParser $ "[42, \"foo\", '$1{}', null, {\"f\":\"v\"}, [1,2,3]]"
        ,test_result = Just . ArrayV $ [
             LitTU (NumV 42)
            ,LitTU (StrV "foo")
            ,StrTU (hole 1)
            ,LitTU NullV
            ,LitTU (ObjV [("f",LitTU (StrV "v"))])
            ,LitTU (ArrayV [LitTU (NumV 1),LitTU (NumV 2),LitTU (NumV 3)])
         ]
    }

test_array7 :: UnitTest (Maybe Value)
test_array7 = UnitTest {
         test_output = testParser arrayVParser $ "[1,   2,  3,\n\t 4   \n\n\t]"
        ,test_result = testParser arrayVParser $ "[1,2,3,4]"
    }

test_array8 :: UnitTest (Maybe Value)
test_array8 = UnitTest {
         test_output = testParser arrayVParser $ "[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]"
        ,test_result = Just . ArrayV $ [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [LitTU (ArrayV [])])])])])])])])])])])])])])])])])])])])])])])])])])])])])])])])])])])]
    }

test_array9 :: UnitTest (Maybe Value)
test_array9 = UnitTest {
         test_output = testParser arrayVParser $ "[}"
        ,test_result = Nothing
    }

test_array10 :: UnitTest (Maybe Value)
test_array10 = UnitTest {
         test_output = testParser arrayVParser $ "[1,2,3,4"
        ,test_result = Nothing
    }

test_array11 :: UnitTest (Maybe Value)
test_array11 = UnitTest {
         test_output = testParser arrayVParser $ "[\"\\u0000\", \"\\uD83D\\uDE00\"]"
        ,test_result = Just . ArrayV $ [LitTU (StrV "\NUL"),LitTU (StrV "😀")]
    }

-- * Objects
test_object1 :: UnitTest (Maybe Value)
test_object1 = UnitTest {
         test_output = testParser objVParser $ "{}"
        ,test_result = Nothing
    }

test_object2 :: UnitTest (Maybe Value)
test_object2 = UnitTest {
         test_output = testParser objVParser $ "{\"field1\" 1}"
        ,test_result = Nothing
    }

test_object3 :: UnitTest (Maybe Value)
test_object3 = UnitTest {
         test_output = testParser objVParser $ "{\"field1\": 1"
        ,test_result = Nothing
    }

test_object4 :: UnitTest (Maybe Value)
test_object4 = UnitTest {
         test_output = testParser objVParser $ "{\"field1\": 1}"
        ,test_result = Just . ObjV $ [("field1",LitTU (NumV 1))]
    }

test_object5 :: UnitTest (Maybe Value)
test_object5 = UnitTest {
         test_output = testParser objVParser $ "{\"\": 1}"
        ,test_result = Just . ObjV $ [("",LitTU (NumV 1))]
    }

test_object6 :: UnitTest (Maybe Value)
test_object6 = UnitTest {
         test_output = testParser objVParser $ "{\"a\": 1, \"a\": 2}"
        ,test_result = Nothing
    }

test_object7 :: UnitTest (Maybe Value)
test_object7 = UnitTest {
         test_output = testParser objVParser $ "\n  \t  \n\n  {\n\t\n\"a\": 1,\t \"a\"  \n\t: \n\t2}"
        ,test_result = Nothing
    }

test_object8 :: UnitTest (Maybe Value)
test_object8 = UnitTest {
         test_output = testParser objVParser $ "{\"\\u006E\\u0061\\u006D\\u0065\": \"\\u004A\\u0053\\u004F\\u004E\"}"
        ,test_result = Just . ObjV $ [("name",LitTU (StrV "JSON"))]
    }

test_object9 :: UnitTest (Maybe Value)
test_object9 = UnitTest {
         test_output = testParser objVParser $ "{\"J\\u0053ON\": \"J\\u0053\\u004F\\u004E \\uD83D\\uDE00 is \\u0067reat\\u0021\"}"
        ,test_result = Just (ObjV [("JSON",LitTU (StrV "JSON \128512 is great!"))])
    }

test_parseFile :: Parser a -> FilePath -> IO (Maybe a)
test_parseFile p file = do
    f <- readFile file
    return $ testParser p (DT.pack f)

test_object10 :: String -> UnitTest Bool
test_object10 (DT.pack->json) = UnitTest {
         test_output = isJust $ testParser objVParser json                     
        ,test_result = True
    } 

test_case :: (Show a, Eq a) 
          => String 
          -> UnitTest a 
          -> SpecWith ()
test_case label t = it label $ (test_output t) `shouldBe` (test_result t)
