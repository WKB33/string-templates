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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeAbstractions #-}
module Data.StringTemplate.TemplateInternal where

import GHC.TypeNats            (type (+)
                               ,Natural
                               ,Nat)
import Data.Type.Natural       (type (>))
import Data.Text               qualified as DT
import Data.Constraint (Dict (..), (\\))
import Data.Constraint.Nat (plusAssociates, plusZero)
import Data.Constraint.Unsafe (unsafeAxiom)

-- | A internal template with `n` holes. 
data ITemplate (n :: Nat) where
  Chunk   :: DT.Text      -> ITemplate 0                         -- ^ Chunk of a string.
  Hole    :: Natural      -> ITemplate 1                         -- ^ A hole. 
  Compose :: ((n1 + n2) > 0) => ITemplate n1 -> ITemplate n2 -> ITemplate (n1 + n2) -- ^ Composition of templates.

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

p1 :: forall n1 n2.Dict ((1 + (n1 + n2)) ~ ((1 + n1) + n2))
p1 = Dict \\ plusAssociates @1 @n1 @n2 

p2 :: forall n1 n2.Dict ((1 + n1) + n2 > 0)
p2 = Dict \\ plusAssociates @1 @n1 @n2 \\ p11 @1 @(n1+n2)

-- TODO: Figure out how to prove this.
p11 :: forall n1 n2.(n1 > 0) => Dict ((n1 + n2) > 0)
p11 = unsafeAxiom

-- TODO: Figure out how to prove this.
p12 :: forall n1 n2.(n2 > 0) => Dict ((n1 + n2) > 0)
p12 = unsafeAxiom

p10 :: Dict (1 > 0)
p10 = Dict

p3 :: forall m n1 n2.Dict ((m + (n1 + n2)) ~ ((m + n1) + n2))
p3 = Dict \\ plusAssociates @m @n1 @n2

p4 :: forall m n1 n2.n1 + n2 > 0 => Dict(m + (n1 + n2) > 0)
p4 = Dict \\ p12 @m @(n1 + n2)

p5 :: forall m n4 n5 n2 n1.n1 ~ (n4 + n5) => Dict (((m + n4) + (n5 + n2)) ~ ((m + n1) + n2))
p5 = Dict \\ plusAssociates @m @n4 @(n5 + n2) \\ plusAssociates @n4 @n5 @n2 \\ plusAssociates @m @n1 @n2

p13 :: forall n4 n5 n2.Dict (n4 + (n5 + n2) ~ (n4 + n5) + n2)
p13 = Dict \\ plusAssociates @n4 @n5 @n2

p6 :: forall n4 n5 n2.n4 + n5 > 0 => Dict (n4 + (n5 + n2) > 0)
p6 = Dict \\ p13 @n4 @n5 @n2 \\ p11 @(n4 + n5) @n2

p7 :: forall m n1 n2.(n1 + n2) > 0 => Dict ((m + n1) + n2 > 0)
p7 = Dict \\ plusAssociates @m @n1 @n2 \\ p12 @m @(n1 + n2)

--((m + n1) + n2) ~ (m + n)
p8 :: forall m n1 n4 n5.n1 ~ n4 + n5 => Dict (((m + n4) + n5) ~ (m + n1))
p8 = Dict \\ plusAssociates @m @n4 @n5 

p9 :: forall m n1 n2 n4 n5.(m ~ (n1 + n2), n2 ~ (n4 + n5)) => Dict (((n1 + n4) + n5) ~ m)
p9 = Dict \\ plusAssociates @n1 @n4 @n5

-- Minimal Template (t):
--   i. t is Hole _
--  ii. t is Chunk _ 
--  ii. flatten t = [t' | t' is Template i, i in [0,1]] where the longest run of
--  Template 1's is 1. That is, Template 1 never follows itself.
--
--
-- Recomposition
-- idea: recomp t1 (Compose t2 t3,t4) ~> recomp t1 (t2,Compose t3 t4)
--       recomp (Compose t1 t2) (t3,t4) ~> recomp t1 (Compose t2 t3,t4) ~> recomp t1 (t2,Compose t3 t4)
-- 
recomp :: forall m n1 n2.(n1 + n2 > 0) 
       => ITemplate m 
       -> (ITemplate n1,ITemplate n2) 
       -> ITemplate (m + n1 + n2)
recomp t1@(Hole _) (t2,t3)              = (Compose t1 $ t2 >+> t3) \\ p1 @n1 @n2 \\ p2 @n1 @n2
recomp (Chunk chk1) ((Chunk chk2),t3)   = (Chunk $ chk1 <> chk2) >+> t3
recomp t1@(Chunk _) (t2@(Hole _),t3)    = Compose t1 $ t2 >+> t3
recomp (Compose @n4 @n5 t1 t2) (t3,t4)  = (recomp @n4 @n5 t1 (t2,Compose t3 t4)) \\ p3 @m @n1 @n2 \\ p4 @n5 @n1 @n2
recomp t1 (Compose @n4 @n5 t2 t3,t4)    = (recomp t1 (t2,t3)) >+> t4 \\ p7 @m @n1 @n2 \\ p8 @m @n1 @n4 @n5

(>+>) :: forall m n.(m+n > 0)
      => ITemplate m 
      -> ITemplate n 
      -> ITemplate (m+n)
t1                                          >+> (Compose @n1 @n2 t2 t3) = recomp t1 (t2,t3) \\ p8 @m @n @n1 @n2
(Compose t1 (Chunk chk1))                   >+> (Chunk chk2)            = t1 >+> (Chunk $ chk1 <> chk2)
(Compose t1 t2@(Chunk _))                   >+> t3@(Hole _)             = recomp t1 (t2,t3)
(Compose t1 t2@(Hole _))                    >+> t3@(Chunk _)            = recomp t1 (t2,t3)
(Compose @n1 t1 (Compose @n4 @n5 t2 t3))    >+> t4                      = recomp t1 (t2,t3) >+> t4 \\ p9 @m @n1 @_ @n4 @n5
t1                                          >+> t2                      = Compose t1 t2

-- | Composition of templates.
-- Use an intermediate list to capture the composition, but when we have a
-- chunck + hole, then pull it out into a Compose to build the Template as we
-- go.
(+>) :: Template
     -> Template
     -> Template
(Template t1) +> (Template t2) = Template $ undefined --t1 >+> t2

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
plug :: Template                     -- ^ Template to plug
     -> (Natural -> Maybe DT.Text)   -- ^ Plug function
     -> Maybe DT.Text
plug (Template t) f = 
    case _plug f t of
        Nothing -> Nothing
        Just (Chunk c) -> Just c

-- | Main logic for plug.
_plug 
    :: (Natural -> Maybe DT.Text) -- ^ Plug function.
    -> ITemplate n                -- ^ ITemplate to plug.
    -> Maybe (ITemplate 0)
_plug f (Hole i) = do
    c <- f i 
    return $ Chunk c
_plug f (Compose t1 t2) = do
    Chunk t1' <- _plug f t1
    Chunk t2' <- _plug f t2
    return $ Chunk $ t1' <> t2'
_plug _ (Chunk t) = return $ Chunk t
