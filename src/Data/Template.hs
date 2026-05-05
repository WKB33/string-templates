{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE RankNTypes #-}
{-|
Module      : Template
Description : Framework for creating string templates
Copyright   : (c) Harley Eades, 2026
              (c) WKB3, 2026
Maintainer  : harley.eades@wkb3.com

Framework for creating string templates
-}
module Data.Template () where
-- External Imports:

import Prelude hiding (const)
import Data.Text qualified as DT
import Data.Kind (Constraint, Type)
import Data.Coerce (coerce)
import GHC.TypeNats (SNat, type (+), KnownNat, type (<=), Natural)
import GHC.TypeLits (Nat)
import Data.Sized (Sized)
import Data.Vector (Vector)
import Data.Type.Ordinal (Ordinal, sNatToOrd, naturalToOrd, inclusion)
import Data.Type.Natural (S)

-- Internal Imports:

{- Example quasi-quoted string into datatype:
{"f1":<1>,"f2":{"f21": <2>,"f22":"foo bar"}}
=> '{"f1":' <> <1> <> ',"f2":{"f21":' <> <2> <> ',"f22":"foo bar"}}'
=> '{"f1":' <> (Hole "1") <> ',"f2":{"f21":' <> (Hole 2) <> ',"f22":"foo bar"}}'
=> (Chunk "{\"f1\":\"") <> (Hole "1") <> (Chunk ",\"f2\":{\"f21\":") <> (Hole "2") <> (Chunk ",\"f22\":\"foo bar\"}}")
-}

data ITemplate (n :: Nat) where
  Hole    :: Natural -> ITemplate 1
  Chunk   :: DT.Text -> ITemplate 0 
  Compose :: (ITemplate n1) -> (ITemplate n2) -> ITemplate (n1 + n2)

instance Show (ITemplate n) where
    show :: ITemplate n -> String
    show (Hole i)        = "Hole" <> (show i)
    show (Chunk t)       = "Chunk" <> (show t)
    show (Compose t1 t2) = "Compose (" <> show t1 <> ") (" <> show t2 <> ")"

(<+>) :: (ITemplate n1) -> (ITemplate n2) -> ITemplate (n1 + n2)
(<+>) = Compose

hole :: Natural
     -> ITemplate 1
hole i = Hole i

chunk :: DT.Text
      -> ITemplate 0
chunk = Chunk

_plugHoles 
    :: (Natural -> DT.Text)
    -> ITemplate n
    -> ITemplate 0
_plugHoles f (Hole i) = Chunk $ f i
_plugHoles f (Compose t1 t2)  = Compose (_plugHoles f t1) (_plugHoles f t2)
_plugHoles _ (Chunk t)        = Chunk t

unpack 
    :: ITemplate 0
    -> DT.Text
unpack = _unpack
    where
        _unpack :: ITemplate n 
               -> DT.Text
        _unpack (Chunk s)       = s        
        _unpack (Compose t1 t2) = _unpack t1 <> _unpack t2
        _unpack (Hole _)        = error "reached unreachable branch"

-- exampleTemp :: ITemplate 3
-- exampleTemp = ((chunk "{\"f1\":\"") 
--         <+> (hole 1) )
--         <+> (chunk (",\"f2\":{\"f21\":")) 
--         <+> (hole 2) 
--         <+> (chunk ",\"f22\":\"foo bar\"}}")

-- plug1 :: Int -> DT.Text
-- plug1 1 = "Staci"
-- plug1 2 = "Jo"
-- plug1 _ = ""