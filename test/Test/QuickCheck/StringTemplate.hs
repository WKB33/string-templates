{-|
Module      : StringTemplate
Description : Generation of random string templates
Copyright   : (c) Harley Eades, 2026
              ) WKB3, 2026
Maintainer  : harley.eades@gmail.com

Includes a generator for QuickCheck to randomly generate string templates to be
used for property-based testing.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Test.QuickCheck.StringTemplate
    (genTemplate) where

import GHC.TypeLits                         (Natural)
import Test.QuickCheck                      (Gen, Arbitrary (arbitrary), generate, frequency, sized)
import Test.QuickCheck.Instances.Text       ()
import Test.QuickCheck.Instances.Natural    ()
import Data.Functor.Identity                (Identity)
import Data.Text                            qualified as DT

import Data.StringTemplate.TemplateInternal

genChunk :: Gen (Template ())
genChunk = chunk <$> arbitrary

genTemplateNat :: Natural -> Gen (Template ())
genTemplateNat 0 = genChunk
genTemplateNat n = do (Template t (hls,fhls)) <- genTemplateNat $ n - 1
                      h <- arbitrary :: Gen Int
                      c <- arbitrary :: Gen DT.Text
                      let t' = ICompose c h t
                      pure $ Template t' (h:hls,fhls)

genTemplate :: Gen (Template ())
genTemplate = arbitrary >>= genTemplateNat 

instance Arbitrary (Template ()) where
    arbitrary :: Gen (Template ())
    arbitrary = genTemplate
