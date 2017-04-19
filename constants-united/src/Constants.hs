-- |
-- Module      : Constants
-- Description : 
-- Copyright   : (c) Jonatan H Sundqvist, 2017
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : stable
-- Portability : portable
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
 
module Constants where

--------------------------------------------------------------------------------------------------------------------------------------------

import qualified Prelude
import           Prelude hiding (Num(..), Floating(..), Fractional(..))

import           Control.Applicative ()

import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- data DefaultMap k v = DefaultMap (Map k v)

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO | - Represent every unique dimension as a separate concrete type (we'll probably need some recursive Unit type for that, and possibly type operators)
-- data Dimension = Length | Time | Mass | Charge | Temperature deriving (Eq, Show, Enum, Bounded)


-- |
data SIBaseUnit = Metre | Second | Kilogram | Ampere | Kelvin deriving (Eq, Show, Ord, Enum, Bounded)


-- |
data Unit base i = Unit (Map base i) deriving (Eq, Show)

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- ×
class Multiply a b where
  type Product a b
  (*) :: a -> b -> Product a b


-- |
class Divide a b where
  type Quotient a b
  (/) :: a -> b -> Quotient a b


-- |
class Exponentiate a b where
  type Power a b
  (**) :: a -> b -> a

--------------------------------------------------------------------------------------------------------------------------------------------

-- | To multiply two units, we find the sum of the exponents of each base unit, and prune those that cancel.
instance (Ord base, Prelude.Num i, Eq i)  => Multiply (Unit base i) (Unit base i) where
  type Product (Unit base i) (Unit base i) = Unit base i
  (*) (Unit a) (Unit b) = Unit . Map.filter (/= 0) $ Map.unionWith (Prelude.+) a b


-- | To divide two units, we first negate the exponents of the divisor, and multiply.
instance (Ord base, Prelude.Num i, Eq i) => Divide (Unit base i) (Unit base i) where
  type Quotient (Unit base i) (Unit base i) = Unit base i
  (/) ua (Unit b) = ua * Unit (Map.map negate b)


-- |
instance Multiply Int (Unit base i) where
  type Product Int (Unit base i) = Unit base i
  (*) a b = b


-- |
instance Multiply (Unit base i) Int where
  type Product (Unit base i) Int = Unit base i
  (*) a b = a


-- |
instance Multiply Integer (Unit base i) where
  type Product Integer (Unit base i) = Unit base i
  (*) a b = b


-- |
instance Multiply (Unit base i) Integer where
  type Product (Unit base i) Integer = Unit base i
  (*) a b = a


-- |
instance Multiply Float (Unit base i) where
  type Product Float (Unit base i) = Unit base i
  (*) a b = b


-- |
instance Multiply (Unit base i) Float where
  type Product (Unit base i) Float = Unit base i
  (*) a b = a


-- |
instance Multiply Double (Unit base i) where
  type Product Double (Unit base i) = Unit base i
  (*) a b = b


-- |
instance Multiply (Unit base i) Double where
  type Product (Unit base i) Double = Unit base i
  (*) a b = a

--------------------------------------------------------------------------------------------------------------------------------------------

-- Let's define some units

one      = Enhet (Map.fromList [])
metre    = Enhet (Map.fromList [(meter,    1)])
second   = Enhet (Map.fromList [(sekund,   1)])
kilogram = Enhet (Map.fromList [(kilogram, 1)])
ampere   = Enhet (Map.fromList [(ampere,   1)])
kelvin   = Enhet (Map.fromList [(kelvin,   1)])

hertz    = sekund**(-1)
newton   = kilogram*meter/sekund**2
pascal   = newton/meter**2
joule    = newton*meter
watt     = joule/sekund
coulomb  = sekund*ampere
volt     = newton*meter/coulomb
farad    = kilogram**(-1)*meter**(-2)*sekund**4*ampere**2 -- TODO: Förenkla
ohm      = kilogram*meter**2*sekund**(-3)*ampere**(-2)    -- TODO: Förenkla
siemens  = kilogram**(-1)*meter**(-2)*sekund**3*ampere**2 -- TODO: Förenkla
weber    = kilogram*meter**2*sekund**(-2)*ampere**(-1)    -- TODO: Förenkla
tesla    = kilogram*sekund**(-2)*ampere**(-1)             -- TODO: Förenkla
henry    = kilogram*meter**2*sekund**(-2)*ampere**(-2)    -- TODO: Förenkla

-- Enheter (kortnamn)
-- m  = meter
-- s  = sekund
-- kg = kilogram
-- A  = ampere
-- K  = kelvin

-- Hz = hertz
-- N  = newton
-- Pa = pascal
-- J  = joule
-- W  = watt
-- C  = coulomb
-- V  = volt
-- # f = farad
-- # o = ohm
-- # s = siemens
-- # w = weber
-- # t = tesla
-- # h = henry

-- # Härledda enheter med särskilda namn
-- # TODO: Härledda storheter med särskilda namn (särskild klass, kanske namedtuple)
-- # TODO: Stöd för flerspråkighet och konfigurering
-- # enhetsnamn = {
-- #   hertz   : ('Hertz'   , 'frekvens'),
-- #   newton  : ('Newton'  , 'kraft'),
-- #   pascal  : ('Pascal'  , 'tryck'),
-- #   joule   : ('Joule'   , 'energi'),
-- #   watt    : ('Watt'    , 'effekt'),
-- #   coulomb : ('Coulomb' , 'laddning'),
-- #   volt    : ('Volt'    , 'spänning'),
-- #   farad   : ('Farad'   , 'kapacitans'),
-- #   ohm     : ('Ohm'     , 'resistans'),
-- #   siemens : ('Siemens' , 'konduktans'),
-- #   weber   : ('Weber'   , 'magnetiskt flöde'),
-- #   tesla   : ('Tesla'   , 'magnetisk flödestäthet'),
-- #   henry   : ('Henry'   , 'induktans')