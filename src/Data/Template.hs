{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-|
Module      : Template
Description : Framework for creating string templates
Copyright   : (c) Harley Eades, 2026
              (c) WKB3, 2026
Maintainer  : harley.eades@wkb3.com

Framework for creating string templates
-}
module Data.Template ((<+>)
                     ,hole
                     ,chunk
                     ,plugHoles
                     ,unpack) where

import GHC.TypeNats (type (+)
                    ,Natural)
import GHC.TypeLits (Nat)

import Data.Text qualified as DT

data Template (n :: Nat) where
  Hole    :: Natural -> Template 1
  Chunk   :: DT.Text -> Template 0 
  Compose :: (Template n1) -> (Template n2) -> Template (n1 + n2)  

instance Show (Template n) where
    show :: Template n -> String
    show (Hole i)        = "Hole" <> (show i)
    show (Chunk t)       = "Chunk" <> (show t)
    show (Compose t1 t2) = "Compose (" <> show t1 <> ") (" <> show t2 <> ")"

(<+>) :: Template n1
      -> Template n2
      -> Template (n1 + n2)
t1 <+> t2 = Compose t1 t2

hole :: Natural -> Template 1
hole i = Hole i

chunk :: DT.Text -> Template 0
chunk = Chunk

plugHoles 
    :: (Natural -> Maybe DT.Text)
    -> Template n
    -> Maybe (Template 0)
plugHoles f (Hole i) = do
    c <- f i 
    return $ Chunk c
plugHoles f (Compose t1 t2) = do
    t1' <- plugHoles f t1
    t2' <- plugHoles f t2
    return $ Compose t1' t2'
plugHoles _ (Chunk t) = return $ Chunk t

unpack 
    :: Template 0
    -> DT.Text
unpack = _unpack
    where
        _unpack :: Template n -> DT.Text
        _unpack (Chunk s)       = s        
        _unpack (Compose t1 t2) = _unpack t1 <> _unpack t2
        _unpack (Hole _)        = error "reached unreachable branch"

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