-- |
-- Module      : Constants
-- Description : 
-- Copyright   : (c) Jonatan H Sundqvist, 2017
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : stable
-- Portability : portable
-- 

-- TODO | - 
--        - 

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}

--------------------------------------------------------------------------------------------------------------------------------------------
 
module Constants where

--------------------------------------------------------------------------------------------------------------------------------------------

import qualified Prelude

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
data BaseUnit = Metre | Second | Kilogram | Ampere | Kelvin deriving (Eq, Show, Enum, Bounded)

-- |
data Unit base i = BaseUnit base | CompositeUnit (Map base i)

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
class Negate a where
  negate :: a -> a


-- |
class Negate a => Add a b where
  type Sum
  (+) :: a -> b -> Sum
  (-) :: a -> b -> Sum
  (-) a b = a + (negate b)
