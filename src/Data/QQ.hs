{-# LANGUAGE DataKinds #-}
module Data.QQ () where

import Data.Template (Template
                     ,hole
                     ,chunk
                     ,(<+>))
import Data.Text qualified as DT


data Token = ESCAPE 
           | HOLE_START 
           | HOLE_END
           | CHUNK DT.Text

tokenize :: DT.Text -> [Token]
tokenize ('$' DT.:< '{' DT.:< r) = _
tokenize l = undefined

parseTemplate :: DT.Text -> Template n
parseTemplate "" = undefined
parseTemplate _ = undefined