{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
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
module Data.Template (Template  
                     -- * Combinators                    
                     ,hole
                     ,chunk
                     ,(<+>)
                     -- * Plugging and Unpacking
                     ,plug
                     ,unpack) where

import GHC.TypeNats (type (+)
                    ,Natural)
import GHC.TypeLits (Nat)

import Data.Text qualified as DT

-- | A template with `n` holes. We do not expose the underlying constructors in
-- favor of using the template combinators.
data Template (n :: Nat) where
  Hole    :: Natural -> Template 1                                 -- ^ Hole with a label.
  Chunk   :: DT.Text -> Template 0                                 -- ^ Chunk of a string.
  Compose :: (Template n1) -> (Template n2) -> Template (n1 + n2)  -- ^ Composition of templates.

instance Show (Template n) where
    show :: Template n -> String
    show (Hole i)        = "${" <> (show i) <> "}"
    show (Chunk t)       = DT.unpack t
    show (Compose t1 t2) = show t1 <> show t2

-- | Composition of templates.
(<+>) :: Template n1
      -> Template n2
      -> Template (n1 + n2)
t1 <+> t2 = Compose t1 t2

-- | A hole is a placeholder for a future string. Holes are indexed by natural
-- numbers.
hole :: Natural -- ^ Hole index.
     -> Template 1
hole i = Hole i

-- | A chunk is a substring to a larger string.
chunk :: DT.Text -- ^ Substring.
      -> Template 0
chunk = Chunk

-- | Plugs every hole in a template using the given plug function. If the plug
-- function is defined for every hole in the input template, then this function
-- guarantees a template with no holes (a string) is returned.
plug 
    :: (Natural -> Maybe DT.Text) -- ^ Plug function.
    -> Template n                 -- ^ Template to plug.
    -> Maybe (Template 0)
plug f (Hole i) = do
    c <- f i 
    return $ Chunk c
plug f (Compose t1 t2) = do
    t1' <- plug f t1
    t2' <- plug f t2
    return $ Compose t1' t2'
plug _ (Chunk t) = return $ Chunk t

-- | Unpacks a template with no holes into a string.
unpack 
    :: Template 0 -- ^ Template to unpack.
    -> DT.Text
unpack = _unpack
    where
        _unpack :: Template n -> DT.Text
        _unpack (Chunk s)       = s        
        _unpack (Compose t1 t2) = _unpack t1 <> _unpack t2
        _unpack (Hole _)        = error "reached unreachable branch"
