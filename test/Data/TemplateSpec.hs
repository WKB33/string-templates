module Data.TemplateSpace () where

import Data.Template (Template)

_exampleTemp :: Template 2
_exampleTemp = (chunk "{\"f1\":\"") 
        <+> (hole 1)
        <+> (chunk (",\"f2\":{\"f21\":")) 
        <+> (hole 2) 
        <+> (chunk ",\"f22\":\"foo bar\"}}")

_plug1 :: Natural -> Maybe DT.Text
_plug1 1 = Just "Staci"
_plug1 2 = Just "Jo"
_plug1 _ = Nothing