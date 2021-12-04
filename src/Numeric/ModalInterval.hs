-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.ModalInterval
-- Copyright   :  (c) 2021 Fabrício Olivetti
-- License     :  BSD3
-- Maintainer  :  fabricio.olivetti@gmail.com
-- Stability   :  experimental
-- Portability :  GADTs, StandaloneDeriving
--
-- Kaucher Interval arithmetic based on `intervals` library by (c) Edward Kmett 2010-2013
-- and https://sites.google.com/site/modalintervalcalculator/
--
-----------------------------------------------------------------------------
module Numeric.ModalInterval
  ( Kaucher
  , (+/-)
  , (<.<)
  , proper
  , improper
  , whole
  , empty
  , isEmpty
  , singleton
  , inf
  , sup
  , dual
  , width
  , isInvalid
  , bisect
  , midpoint  
  , member
  , notMember
  , (|^|)
  , (|^^|)
  , intersection
  , hull
  , meet
  , join
  , contains
  , isSubsetOf
  ) where

import Numeric.ModalInterval.Internal
