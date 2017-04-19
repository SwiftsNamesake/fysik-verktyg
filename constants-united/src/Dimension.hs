-- |
-- Module      : Dimension
-- Description : 
-- Copyright   : (c) Jonatan H Sundqvist, 2017
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental
-- Portability : GHC
-- 

-- TODO | - Compatibility layer (eg. a module that defines a bridge between this module and the Prelude)
--        - Parsing
--        - User-defined units
--        - Preserving the order (should we care about that?)
--        - Scalars and vectors

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveFunctor         #-}

--------------------------------------------------------------------------------------------------------------------------------------------
 
module Dimension where

--------------------------------------------------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO | - Represent every unique dimension as a separate concrete type (we'll probably need some recursive Unit type for that, and possibly type operators)
-- data Dimension = Length | Time | Mass | Charge | Temperature deriving (Eq, Show, Enum, Bounded)


-- |
-- data SIBaseUnit = Metre | Second | Kilogram | Ampere | Kelvin deriving (Eq, Show, Ord, Enum, Bounded)


-- |
-- data Unit base i = Unit (Map base i) deriving (Eq, Show)

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO | - Should we really include amount of substance? The SI system does, but it's superfluous since we could encode it as a multiple of
--          the dimensionless number 1.
--        - Speaking of which, is luminous intensity really a fundamental dimension?
data Dimension i = Dimension {
                     fLength       :: i!,
                     fTime         :: i!,
                     fMass         :: i!,
                     fCharge       :: i!,
                     fTemperature  :: i!,
                     fAmountOfSubstance :: i!,
                     fLuminousIntensity :: i!
                   } deriving (Show, Eq)

--------------------------------------------------------------------------------------------------------------------------------------------

length :: Integral i => Dimension i
length = Dimension 1 0 0 0 0 0 0

time :: Integral i => Dimension i
time = Dimension 0 1 0 0 0 0 0

mass :: Integral i => Dimension i
mass = Dimension 0 0 1 0 0 0 0

charge :: Integral i => Dimension i
charge = Dimension 0 0 0 1 0 0 0

temperature :: Integral i => Dimension i
temperature = Dimension 0 0 0 0 1 0 0

amountOfSubstance :: Integral i => Dimension i
amountOfSubstance = Dimension 0 0 0 0 0 1 0

luminousIntensity :: Integral i => Dimension i
luminousIntensity = Dimension 0 0 0 0 0 0 1

