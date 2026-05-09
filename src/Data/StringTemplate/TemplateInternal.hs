{-|
Module      : Template
Description : Framework for creating string templates
Copyright   : (c) Harley Eades, 2026
              (c) WKB3, 2026
Maintainer  : harley.eades@wkb3.com

Framework for creating string templates. These are strings with holes that 
can be plugged. No parsing of the actual string is done, but the string is 
broken up into `chunk`'s in between the `hole`'s. Then a plug function can 
be defined to replace the holes with strings; see `plug`.
-}
{-# LANGUAGE DataKinds                    #-}
{-# LANGUAGE TypeOperators                #-}
{-# LANGUAGE AllowAmbiguousTypes          #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE ScopedTypeVariables          #-}
{-# LANGUAGE RankNTypes                   #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Data.StringTemplate.TemplateInternal where

import GHC.TypeNats            (type (+)
                               ,Natural
                               ,Nat)
import Data.Type.Natural       (S)
import Data.Text               qualified as DT

-- | A internal template with `n` holes. 
data ITemplate (n :: Nat) where
  Chunk   :: DT.Text      -> ITemplate 0                         -- ^ Chunk of a string.
  Hole    :: Natural      -> ITemplate (S n)                     -- ^ A hole. 
  Compose :: ITemplate n1 -> ITemplate n2 -> ITemplate (n1 + n2) -- ^ Composition of templates.

instance Show (ITemplate n) where
    show :: ITemplate n -> String    
    show (Chunk t)       = DT.unpack t
    show (Hole h)        = "${" <> show h <> "}"
    show (Compose t1 t2) = (show t1) <> (show t2)

-- | A template with pluggable holes. We do not expose the underlying
-- constructors in favor of the combinators.
data Template where
    Template :: ITemplate m -> Template

instance Show Template where
    show :: Template -> String
    show (Template t) = show t

-- | Composition of templates.
(+>) :: Template
     -> Template
     -> Template
(Template t1) +> (Template t2) = Template $ Compose t1 t2

-- | A hole.
hole :: Natural
     -> Template
hole = Template . Hole

-- | A chunk is a substring to a larger string.
chunk :: DT.Text -- ^ Substring.
      -> Template
chunk = Template . Chunk

-- | Convert a template into a `Text`, but in AST form rather than pretty
-- printing. The `Show` instance for `Template` is set to pretty print, but for
-- debugging it is sometimes useful to see the raw AST.
showAST :: Template -> DT.Text
showAST (Template (Chunk chk))     = "Chunk " <> DT.show chk
showAST (Template (Hole h))        = "Hole "  <> (DT.show h)
showAST (Template (Compose t1 t2)) = "Compose (" 
                                  <> showAST (Template t1) 
                                  <> ") ("
                                  <> showAST (Template t2)
                                  <> ")"

-- | Plugs every hole in a template using the given plug function. If the plug
-- function is defined for every hole in the input template, then this function
-- guarantees a template with no holes (a string) is returned.
plug :: (Natural -> Maybe DT.Text)
     -> Template
     -> Maybe DT.Text
plug f (Template t) = 
    case _plug f t of
        Nothing -> Nothing
        Just (Chunk c) -> Just c
        _ -> error "impossible branch"

-- | Main logic for plug.
_plug 
    :: (Natural -> Maybe DT.Text) -- ^ Plug function.
    -> ITemplate n                -- ^ ITemplate to plug.
    -> Maybe (ITemplate 0)
_plug f (Hole i) = do
    c <- f i 
    return $ Chunk c
_plug f (Compose t1 t2) = do
    t1' <- _plug f t1
    t2' <- _plug f t2
    return $ Compose t1' t2'
_plug _ (Chunk t) = return $ Chunk t
