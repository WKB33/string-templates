{-|
Module      : Parser
Description : Parser for string templates
Copyright   : (c) Harley Eades, 2026
              (c) WKB3, 2026
Maintainer  : harley.eades@gmail.com

[Megaparsec](https://hackage.haskell.org/package/megaparsec) parser for string
templates.
-}
{-# LANGUAGE PatternSynonyms #-}
module Data.StringTemplate.Parser (
     -- * Parsing Templates
     parseTemplate
    ,templateParser
    ,templateParserTest) where

import Text.Megaparsec                      (Parsec
                                            ,between
                                            ,skipCount
                                            ,many
                                            ,some
                                            ,satisfy
                                            ,choice
                                            ,atEnd
                                            ,parseTest
                                            ,parse
                                            ,errorBundlePretty)
import Data.Char                            qualified as DT
import Data.Text                            qualified as DT
import Data.Void                            (Void)
import Text.Megaparsec.Char                 (char
                                            ,digitChar)
import Numeric.Natural                      (Natural)
import Data.StringTemplate.TemplateInternal (pattern Chunk
                                            ,pattern Compose
                                            ,Hole
                                            ,Template)

-- | Parse a template.
parseTemplate :: DT.Text -> Either DT.Text Template
parseTemplate s 
    = case parse templateParser "" s of
        Left bundle -> Left . DT.pack $ "string-templates: " <> errorBundlePretty bundle
        Right t -> Right t

-- | Convenient function for testing the parser in GHCi.
templateParserTest :: DT.Text -> IO ()
templateParserTest = parseTest templateParser

-- | The parser operates on a stream of `Text`.
type Parser = Parsec Void DT.Text

-- | Parse a hole index (`Natural`).
holeIndexParser :: Parser Natural
holeIndexParser = do
    ds <- some digitChar
    pure . read $ ds

-- | Parse a hole's filling which must be escaped properly.
holeFillingParser :: Parser (Maybe DT.Text)
holeFillingParser = do 
    f <- between (char '{') (char '}') $ many (templateCharParser True)
    pure $ if null f
         then Nothing
         else Just . DT.pack $ f

-- | Parse a `Hole`. That is, a pair of a hole index and a filling.
holeParser :: Parser Hole
holeParser = do
    skipCount 1 (char '$')
    i <- holeIndexParser
    f <- holeFillingParser
    pure $ (i, f)

-- | Parse a `Chunk`.
chunkParser :: Parser DT.Text
chunkParser = DT.pack <$> many (templateCharParser False)

-- | Parse a template either as a `Chunk` or a `Compose`.
templateParser :: Parser Template
templateParser = do
    mc <- chunkParser
    isEnd <- atEnd
    if isEnd
    then pure . Chunk $ mc
    else do h <- holeParser
            t <- templateParser
            pure $ Compose mc h t

-- | Parse a template character. These are any unicode character where the
-- characters @['$','{','}','\\']@ are escaped when parsing a hole's filling,
-- otherwise just @'$'@ needs to be escaped.
templateCharParser :: Bool -> Parser DT.Char
templateCharParser filling = choice [
        satisfy (\c -> c /= '$' && c /= '\'' && (if filling then c /= '{' && c /= '}' else True) && c /= '\\'),
        escapedTemplateCharParser
    ]

-- | Parsed an escaped character; one of, @["\\$"","\\{"","\\}","\\\\"]@.
escapedTemplateCharParser :: Parser Char
escapedTemplateCharParser = do
    skipCount 1 (char '\\')
    satisfy (\c -> c == '$' || c == '{' || c == '}' || c == '\'')

