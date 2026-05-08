{-|
Module      : Quasi-Quoter for Templates
Description : Framework for creating string templates
Copyright   : (c) Harley Eades, 2026
              (c) WKB3, 2026
Maintainer  : harley.eades@wkb3.com

We include parsers for templates as well as a quasi-quoter 
for generating templates at compile time.
-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE ViewPatterns       #-}
module Data.QQ (parseTemplate) where

import Data.Template (Template
                     ,hole
                     ,(+>)
                     ,chunk)
import GHC.Natural   (Natural)
import GHC.Unicode   (isDigit)
import Data.Text     qualified as DT

parseHole :: DT.Text -> (Either DT.Text Natural,DT.Text)
parseHole ('{' DT.:< r) = 
    case DT.span isDigit r of
        (DT.Empty,_           ) -> (Left $ "invalid hole label expecting a number",r)
        (d       ,'}' DT.:< r') -> (Right . read . DT.unpack $ d,r')
        (d       ,x   DT.:< r') -> (Left $ "found hole label "<>d<>" but encountered an invalid symbol "<>(DT.singleton x),r')
        (d       ,DT.Empty    ) -> (Left $ "found hole label "<>d<>" but encountered an unexpected end of input",DT.Empty)
parseHole (x   DT.:< r) = (Left $ "invalid symbol "<>DT.singleton x<>" expecting {",r)
parseHole DT.Empty      = (Left "unexpected end of input",DT.Empty)

parseEscape :: DT.Text -> (Either DT.Text DT.Text,DT.Text)
parseEscape ('$' DT.:< r) = (Right "$",r)
parseEscape (x   DT.:< r) = (Left $ "invalid escaped symbol "<>DT.singleton x<>" expecting $",r)
parseEscape DT.Empty      = (Left "unexpected end of input",DT.Empty)

-- | Parse a template from a string. 
parseTemplate :: DT.Text -> Either DT.Text Template
-- Parse a hole
parseTemplate ('$'  DT.:< r) = do
    let (h',r') = parseHole r
    h <- h'
    if DT.null r'
    then return $ (hole h)
    else do t <- parseTemplate r'
            return $ (hole h) +> t
-- Parse escape
parseTemplate ('\\' DT.:< r) = do
    let (chk',r') = parseEscape r
    chk <- chk'
    if DT.null r'
    then return $ (chunk chk)
    else do t <- parseTemplate r'
            return $ (chunk chk) +> t
-- Parse chunk
parseTemplate (x    DT.:< r) = 
    if DT.null r
    then return $ (chunk (DT.singleton x))
    else do t <- parseTemplate r 
            return $ (chunk (DT.singleton x)) +> t

parseTemplate DT.Empty = Left "unexpected end of input"