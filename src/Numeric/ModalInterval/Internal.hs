{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.ModalInterval.Internal
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
module Numeric.ModalInterval.Internal
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

import Numeric.ModalInterval.Exception
import Control.Exception as Exception
import Data.SRTree (OptIntPow(..))

-- | Kaucher arithmetic can describe a proper (forall) and improper (exists)
-- intervals with different semantics.
--
-- The interval `K x y` is proper if `x >= y` and improper otherwise.
data Kaucher a where
    K               :: RealFloat a => !a -> !a -> Kaucher a
    EmptyInterval   :: RealFloat a => Kaucher a 
    InvalidInterval :: RealFloat a => Kaucher a

deriving instance Eq a => Eq (Kaucher a)
deriving instance Ord a => Ord (Kaucher a)

instance (Show a, Ord a) => Show (Kaucher a) where
    showsPrec _ (K x y )  | x <= y    = showString "∀ x . x ∈ [" . showsPrec 3 x . showString ", " . showsPrec 3 y . showString "]"
                          | otherwise = showString "∃ x . x ∈ [" . showsPrec 3 x . showString ", " . showsPrec 3 y . showString "]"
    showsPrec _ EmptyInterval         = showString "∅"
    showsPrec _ InvalidInterval       = showString "NaN"

(+/-) :: RealFloat a => a -> a -> Kaucher a
a +/- b | isNaN a || isNaN b = InvalidInterval
        | otherwise          = K (a - b) (a + b)
{-# INLINE (+/-) #-}
infixl 6 +/-

proper :: Ord a => Kaucher a -> Kaucher a 
proper (K x y)         = K (min x y) (max x y)
proper EmptyInterval   = EmptyInterval
proper InvalidInterval = InvalidInterval
{-# INLINE proper #-}

improper :: Ord a => Kaucher a -> Kaucher a 
improper (K x y)         = K (max x y) (min x y)
improper EmptyInterval   = EmptyInterval
improper InvalidInterval = InvalidInterval
{-# INLINE improper #-}

-- | Create open and closed proper intervals
-- 
-- >>> 1 <=.<= 3
-- ∀ x . x ∈ [1, 3]
-- 
-- >>> 3 <=.<= 1
-- ∃ x . x ∈ [1, 3]
(<.<) :: RealFloat a => a -> a -> Kaucher a
a <.< b | isNaN a || isNaN b = InvalidInterval
        | otherwise          = K a b
{-# INLINE (<.<) #-}
infix 3 <.<

singleton :: RealFloat a => a -> Kaucher a
singleton x | isNaN x = InvalidInterval
            | otherwise = K x x
{-# INLINE singleton #-}

whole :: RealFloat a => Kaucher a 
whole = K (-1/0) (1/0)
{-# INLINE whole #-}

empty :: RealFloat a => Kaucher a
empty = EmptyInterval
{-# INLINE empty #-}

isEmpty :: Kaucher a -> Bool
isEmpty EmptyInterval = True
isEmpty _             = False
{-# INLINE isEmpty #-}

inf :: Kaucher a -> Maybe a
inf (K a _)  = Just a
inf _        = Nothing
{-# INLINE inf #-}
sup :: Kaucher a -> Maybe a
sup (K _ b)  = Just b
sup _        = Nothing
{-# INLINE sup #-}

dual :: Kaucher a -> Kaucher a
dual (K a b)         = K b a
dual EmptyInterval   = EmptyInterval
dual InvalidInterval = InvalidInterval
{-# INLINE dual #-}

width :: Num a => Kaucher a -> a
width (K a b)  = abs $ b - a
width _        = 0
{-# INLINE width #-}

isInvalid :: Kaucher a -> Bool
isInvalid InvalidInterval = True
isInvalid _ = False
{-# INLINE isInvalid #-}

-- | Bisect an interval at its midpoint.
--
-- >>> bisect (10.0 ... 20.0)
-- (10.0 ... 15.0,15.0 ... 20.0)
--
-- >>> bisect (singleton 5.0)
-- (5.0 ... 5.0,5.0 ... 5.0)
--
-- >>> bisect Empty
-- (Empty,Empty)
bisect :: Fractional a => Kaucher a -> (Kaucher a, Kaucher a)
bisect EmptyInterval = (EmptyInterval, EmptyInterval)
bisect InvalidInterval = (InvalidInterval, InvalidInterval)
bisect (K a b) = (K a m, K m b) where m = a + (b - a) / 2
{-# INLINE bisect #-}

-- | Nearest point to the midpoint of the interval.
--
-- >>> midpoint (10.0 ... 20.0)
-- 15.0
--
-- >>> midpoint (singleton 5.0)
-- 5.0
--
-- >>> midpoint empty
-- *** Exception: empty interval
midpoint :: Fractional a => Kaucher a -> a
midpoint (K a b) = a + (b - a) / 2
midpoint EmptyInterval   = Exception.throw (EmptyIntervalError "midpoint")
midpoint InvalidInterval = Exception.throw (InvalidIntervalError "midpoint")
{-# INLINE midpoint #-}

-- | Determine if a point is in the interval.
--
-- >>> member 3.2 (1.0 ... 5.0)
-- True
--
-- >>> member 5 (1.0 ... 5.0)
-- True
--
-- >>> member 1 (1.0 ... 5.0)
-- True
--
-- >>> member 8 (1.0 ... 5.0)
-- False
--
-- >>> member 5 empty
-- False
--
member :: Ord a => a -> Kaucher a -> Bool
member x (K a b) = x >= a && x <= b
member x EmptyInterval   = False -- Exception.throw EmptyIntervalError
member x InvalidInterval = False -- Exception.throw InvalidIntervalError
{-# INLINE member #-}

-- | Determine if a point is not included in the interval
--
-- >>> notMember 8 (1.0 ... 5.0)
-- True
--
-- >>> notMember 1.4 (1.0 ... 5.0)
-- False
--
-- And of course, nothing is a member of the empty interval.
--
-- >>> notMember 5 empty
-- True
notMember :: Ord a => a -> Kaucher a -> Bool
notMember x xs = not (member x xs)
{-# INLINE notMember #-}

(|^|) :: (RealFloat a, Ord a, Integral b) => Kaucher a -> b -> Kaucher a
EmptyInterval |^| _ = EmptyInterval
y |^| 0        = singleton 1
y |^| 1        = y
(K a b) |^| k 
  | k < 0            = error "negative exponent in (a...b)^k"
  | odd k            = (a ^ k) <.< (b ^ k)
  | a >= 0 && b >= 0 = (a ^ k) <.< (b ^ k)
  | a <  0 && b <  0 = (b ^ k) <.< (a ^ k)
  | a <  0 && b >= 0 = 0 <.< (max (a ^ k) (b ^ k))
  | a >= 0 && b <  0 = (max (a ^ k) (b ^ k)) <.< 0
{-# INLINE (|^|) #-}
  
(|^^|) :: (RealFloat a, Ord a, Integral b) => Kaucher a -> b -> Kaucher a  
EmptyInterval |^^| _ = EmptyInterval
y |^^| k
  | k < 0     = recip (y |^| (-k))
  | otherwise = y |^| k
{-# INLINE (|^^|) #-}
infixr 8 |^|
infixr 8 |^^|

-- | Calculate the intersection of two intervals.
--
-- >>> intersection (1 ... 10 :: Interval Double) (5 ... 15 :: Interval Double)
-- 5.0 ... 10.0
intersection :: (RealFloat a, Ord a) => Kaucher a -> Kaucher a -> Kaucher a
intersection InvalidInterval _ = InvalidInterval
intersection _ InvalidInterval = InvalidInterval
intersection k1@(K _ _) k2@(K _ _)
  | b < x || a > y = EmptyInterval
  | otherwise        = (max a x) <.< (min b y)
  where
    K a b = proper k1
    K x y = proper k2
intersection _ _ = EmptyInterval
{-# INLINE intersection #-}

-- | Calculate the convex hull of two intervals
--
-- >>> hull (0 ... 10 :: Interval Double) (5 ... 15 :: Interval Double)
-- 0.0 ... 15.0
--
-- >>> hull (15 ... 85 :: Interval Double) (0 ... 10 :: Interval Double)
-- 0.0 ... 85.0
hull :: Ord a => Kaucher a -> Kaucher a -> Kaucher a
hull (K a b) (K a' b') = (min a a') <.< (max b b')
hull EmptyInterval x = x
hull x EmptyInterval = x
hull _ _ = Exception.throw (InvalidIntervalError "hull")
{-# INLINE hull #-}

meet :: (RealFloat a, Ord a) => [Kaucher a] -> Kaucher a
meet = foldr hull EmptyInterval
{-# INLINE meet #-}

join :: (RealFloat a, Ord a) => [Kaucher a] -> Kaucher a
join = foldr1 intersection
{-# INLINE join #-}

-- | Check if interval @X@ totally contains interval @Y@
--
-- >>> (20 ... 40 :: Interval Double) `contains` (25 ... 35 :: Interval Double)
-- True
--
-- >>> (20 ... 40 :: Interval Double) `contains` (15 ... 35 :: Interval Double)
-- False
contains :: Ord a => Kaucher a -> Kaucher a -> Bool
contains _ EmptyInterval = True
contains (K ax bx) (K ay by) = ax <= ay && by <= bx
contains EmptyInterval K{} = False
contains InvalidInterval _ = False
contains _ InvalidInterval = False
{-# INLINE contains #-}

-- | Flipped version of `contains`. Check if interval @X@ a subset of interval @Y@
--
-- >>> (25 ... 35 :: Interval Double) `isSubsetOf` (20 ... 40 :: Interval Double)
-- True
--
-- >>> (20 ... 40 :: Interval Double) `isSubsetOf` (15 ... 35 :: Interval Double)
-- False
isSubsetOf :: Ord a => Kaucher a -> Kaucher a -> Bool
isSubsetOf = flip contains
{-# INLINE isSubsetOf #-}

-- Supporting functions (not exported)

negInfinity, posInfinity :: Floating a => a
negInfinity = -1 / 0
posInfinity =  1 / 0
{-# INLINE negInfinity #-}
{-# INLINE posInfinity #-}

isOpen :: Kaucher a -> Bool
isOpen (K a b) | isInfinite a || isInfinite b = True
isOpen _ = False
{-# INLINE isOpen #-}

fmod :: (RealFloat a) => Kaucher a -> Kaucher a -> Kaucher a
fmod EmptyInterval _ = EmptyInterval
fmod _ EmptyInterval = EmptyInterval
fmod InvalidInterval _ = InvalidInterval
fmod _ InvalidInterval = InvalidInterval
fmod a b = a - q*b where
  q = rtrunc (a / b)
  rtrunc EmptyInterval = EmptyInterval
  rtrunc InvalidInterval = InvalidInterval
  rtrunc z@(K x y) | isInfinite x && isInfinite y = whole
                   | isInfinite x = x <.< (realToFrac $ truncate y)
                   | isInfinite y = (realToFrac $ truncate x) <.< y
                   | otherwise    = realToFrac $ truncate z
  
{-# INLINE fmod #-}

-- | lift a monotone increasing function over a given interval
increasing :: (RealFloat a, RealFloat b) => (a -> b) -> Kaucher a -> Kaucher b
increasing f (K a b)       = (f a) <.< (f b)
increasing _ EmptyInterval = EmptyInterval
increasing _ InvalidInterval = InvalidInterval

-- | lift a monotone decreasing function over a given interval
decreasing :: (RealFloat a, RealFloat b) => (a -> b) -> Kaucher a -> Kaucher b
decreasing f (K a b)       = (f b) <.< (f a)
decreasing _ EmptyInterval = EmptyInterval
decreasing _ InvalidInterval = InvalidInterval

-- | dualize a function 
dualize :: (Kaucher a -> Kaucher a) -> Kaucher a -> Kaucher a
dualize f = dual . f . dual

instance (Num a, Ord a, RealFloat a) => Num (Kaucher a) where
  K a b + K x y             = (a + x) <.< (b + y)
  k1 + k2 | isInvalid k1 || isInvalid k2 = InvalidInterval
          | otherwise                    = EmptyInterval
  {-# INLINE (+) #-}
  K a b - K x y = (a - y) <.< (b - x)
  k1 - k2 | isInvalid k1 || isInvalid k2 = InvalidInterval
          | otherwise                    = EmptyInterval
  {-# INLINE (-) #-}
  k1@(K a b) * k2@(K x y)
    | isOpen k1 && 0 `member` k2                   = InvalidInterval
    | isOpen k2 && 0 `member` k1                   = InvalidInterval
    | a >= 0 && b >= 0                           = case1mul -- forall and exists
    | a >= 0 && b <  0                           = case2mul -- exists
    | a <  0 && b >= 0                           = case3mul -- forall
    | otherwise                                  = case4mul -- forall and exists
    where
      case1mul | x >= 0 && y >= 0 = (a * x) <.< (b * y)
               | x >= 0 && y <  0 = (a * x) <.< (a * y)
               | x <  0 && y >= 0 = (b * x) <.< (b * y)
               | x <  0 && y <  0 = (b * x) <.< (a * y)                              
      case2mul | x >= 0 && y >= 0 = (a * x) <.< (b * x)
               | x >= 0 && y <  0 = (max (a * x) (b * y)) <.< (min (a * y) (b * x))
               | x <  0 && y >= 0 = 0 <.< 0
               | x <  0 && y <  0 = (b * y) <.< (a * x)
      case3mul | x >= 0 && y >= 0 = (a * y) <.< (b * y)
               | x >= 0 && y <  0 = 0 <.< 0 
               | x <  0 && y >= 0 = (min (a * y) (b * x)) <.< (max (a * x) (b * y))
               | x <  0 && y <  0 = (b * x) <.< (a * x)                              
      case4mul | x >= 0 && y >= 0 = (a * y) <.< (b * x)
               | x >= 0 && y <  0 = (b * y) <.< (b * x)
               | x <  0 && y >= 0 = (a * y) <.< (a * x)
               | x <  0 && y <  0 = (b * y) <.< (a * x)                       
  k1 * k2 | isInvalid k1 || isInvalid k2 = InvalidInterval
          | otherwise                    = EmptyInterval
  {-# INLINE (*) #-}
  abs x@(K a b)
    | a >= 0 && b >= 0 = x
    | a < 0  && b <  0 = negate x
    | a < 0  && b >= 0 = 0 <.< max (- a) b
    | a >= 0 && b <  0 = (max a (- b)) <.< 0
  abs EmptyInterval    = EmptyInterval
  abs InvalidInterval  = InvalidInterval
  {-# INLINE abs #-}

  signum = increasing signum
  {-# INLINE signum #-}

  fromInteger i = singleton (fromInteger i)
  {-# INLINE fromInteger #-}
 
-- | 'realToFrac' will use the midpoint
instance RealFloat a => Real (Kaucher a) where
  toRational EmptyInterval   = Exception.throw (EmptyIntervalError "toRational")
  toRational InvalidInterval = Exception.throw (InvalidIntervalError "toRational")
  toRational (K ra rb) = a + (b - a) / 2 where
    a = toRational ra
    b = toRational rb
  {-# INLINE toRational #-}

instance (RealFloat a, Ord a) => Fractional (Kaucher a) where
  EmptyInterval / _  = EmptyInterval
  _ / EmptyInterval  = EmptyInterval
  InvalidInterval / _ = InvalidInterval
  _ / InvalidInterval = InvalidInterval
  k1@(K a b) / k2@(K x y)
    | isNaN a || isNaN b || isNaN x || isNaN y = InvalidInterval
    | (x > 0 && y < 0) || (x < 0 && y > 0) || (x == 0 && y == 0) ||  0 `member` (proper k2) = InvalidInterval
    | a >= 0 && b >= 0    = case1div
    | a >= 0 && b <  0    = case2div
    | a <  0 && b >= 0    = case3div
    | a <  0 && b <  0    = case4div
    where
      case1div | x >  0 && y >  0 =   (a/y) <.< (b/x)
               | x <  0 && y <  0 =   (b/y) <.< (a/x)
               | x >  0 && y == 0 =   posInfinity <.< (b/x)
               | x == 0 && y >  0 =   (a/y) <.< posInfinity
               | x <  0 && y == 0 =   negInfinity <.< (a/x)
               | x == 0 && y <  0 =   (b/y) <.< negInfinity
      case2div | x >  0 && y >  0 =   (a/y) <.< (b/y)
               | x <  0 && y <  0 =   (b/x) <.< (a/x)
               | x >  0 && y == 0 =   posInfinity <.< negInfinity
               | x == 0 && y >  0 =   (a/y) <.< (b/y)
               | x <  0 && y == 0 =   (b/x) <.< (a/x)
               | x == 0 && y <  0 =   posInfinity <.< negInfinity
      case3div | x >  0 && y >  0 =   (a/x) <.< (b/x)
               | x <  0 && y <  0 =   (b/y) <.< (a/y)
               | x >  0 && y == 0 =   (a/x) <.< (b/x)
               | x == 0 && y >  0 =   negInfinity <.< posInfinity
               | x <  0 && y == 0 =   negInfinity <.< posInfinity
               | x == 0 && y <  0 =   (b/y) <.< (a/y)
      case4div | x >  0 && y >  0 =   (a/x) <.< (b/y)
               | x <  0 && y <  0 =   (b/x) <.< (a/y)
               | x >  0 && y == 0 =   (a/x) <.< negInfinity
               | x == 0 && y >  0 =   negInfinity <.< (b/y)
               | x <  0 && y == 0 =   (b/x) <.< posInfinity
               | x == 0 && y <  0 =   posInfinity <.< (a/y)
  {-# INLINE (/) #-}
  recip EmptyInterval   = InvalidInterval
  recip InvalidInterval = InvalidInterval
  recip y@(K 0 0)       = InvalidInterval
  recip y@(K a 0)       = negInfinity <.< (1.0/a)
  recip y@(K 0 b)       = (1.0/b) <.< posInfinity
  recip y@(K a b) | 0 `notMember` y = (1.0/b) <.< (1.0/a)
                  | otherwise     = InvalidInterval
  {-# INLINE recip #-}
  fromRational r  = singleton $ fromRational r
  {-# INLINE fromRational #-}

instance RealFloat a => RealFrac (Kaucher a) where
  properFraction x = (b, x - fromIntegral b)
    where
      b = truncate (midpoint x)
  {-# INLINE properFraction #-}
  ceiling (K a b) = ceiling b
  ceiling EmptyInterval   = Exception.throw (EmptyIntervalError "ceiling")
  ceiling InvalidInterval = Exception.throw (InvalidIntervalError "ceiling")
  {-# INLINE ceiling #-}
  floor (K a b) = floor a
  floor EmptyInterval   = Exception.throw (EmptyIntervalError "floor")
  floor InvalidInterval = Exception.throw (InvalidIntervalError "floor")
  {-# INLINE floor #-}
  round EmptyInterval   = Exception.throw (EmptyIntervalError "round")
  round InvalidInterval = Exception.throw (InvalidIntervalError "round")
  round x               = round (midpoint x)  
  {-# INLINE round #-}
  truncate EmptyInterval   = Exception.throw (EmptyIntervalError "truncate")
  truncate InvalidInterval = Exception.throw (InvalidIntervalError "truncate")
  truncate x               = truncate (midpoint x)
  {-# INLINE truncate #-}

instance (RealFloat a, Ord a) => Floating (Kaucher a) where
  pi = singleton pi
  {-# INLINE pi #-}
  exp EmptyInterval   = EmptyInterval
  exp InvalidInterval = InvalidInterval
  exp x = increasing exp x
  {-# INLINE exp #-}
  log (K a b) | a > 0 && b > 0 = (log a) <.< (log b)
              | otherwise      = InvalidInterval
  log EmptyInterval = EmptyInterval
  log InvalidInterval = InvalidInterval
  {-# INLINE log #-}
  cos EmptyInterval   = EmptyInterval
  cos InvalidInterval = InvalidInterval
  cos x@(K a b) | a > b = dualize cos x -- for improper intervals
                | isOpen x = InvalidInterval
  cos x 
    | width t >= pi = (-1) <.< 1
    | a >= pi = - cos (t - pi)
    | a < 0 = - cos (t + pi)
    | b <= pi = decreasing cos t
    | b <= 2 * pi = (-1) <.<  cos ((pi * 2 - a) `min` b)
    | otherwise = (-1) <.< 1
    where
      t@(K a b) = fmod x (pi * 2)
  {-# INLINE cos #-}
  sin EmptyInterval   = EmptyInterval
  sin InvalidInterval = InvalidInterval
  sin x = cos (x - pi / 2)
  {-# INLINE sin #-}
  tan EmptyInterval   = EmptyInterval
  tan InvalidInterval = InvalidInterval
  tan x@(K a b) | a > b = dualize tan x -- for improper intervals
                | isOpen x = InvalidInterval
  tan x
    | a <= - pi / 2 || b >= pi / 2 = whole
    | otherwise = increasing tan x
    where
      t = fmod x pi
      K a b | t >= pi / 2 = t - pi
            | otherwise    = t
  {-# INLINE tan #-}
  asin EmptyInterval   = EmptyInterval
  asin InvalidInterval = InvalidInterval
  asin x@(K a b) | a > b = dualize asin x
  asin (K a b)
    | a < -1 || b > 1 = EmptyInterval
    | b < -1 || a > 1 = EmptyInterval
    | otherwise = K (if a <= -1 then -halfPi else asin a) (if b >= 1 then halfPi else asin b)
    where
      halfPi = pi / 2
  {-# INLINE asin #-}
  acos EmptyInterval   = EmptyInterval
  acos InvalidInterval = InvalidInterval
  acos x@(K a b) | a > b = dualize acos x  
  acos (K a b)
    | a < -1 || b > 1 = EmptyInterval
    | b < -1 || a > 1 = EmptyInterval
    | otherwise = K (if b >= 1 then 0 else acos b) (if a < -1 then pi else acos a)
  {-# INLINE acos #-}  
  atan EmptyInterval   = EmptyInterval
  atan InvalidInterval = InvalidInterval 
  atan x = increasing atan x
  {-# INLINE atan #-}
  sinh EmptyInterval   = EmptyInterval
  sinh InvalidInterval = InvalidInterval 
  sinh k = increasing sinh k
  {-# INLINE sinh #-}
  cosh EmptyInterval   = EmptyInterval
  cosh InvalidInterval = InvalidInterval 
  cosh x@(K a b) | a > b = dualize cosh x    
  cosh x@(K a b)
    | b < 0  = decreasing cosh x
    | a >= 0 = increasing cosh x
    | otherwise  = 0 <.< cosh (if - a > b
                                 then a
                                 else b)
  {-# INLINE cosh #-}
  tanh EmptyInterval   = EmptyInterval
  tanh InvalidInterval = InvalidInterval 
  tanh k = increasing tanh k
  {-# INLINE tanh #-}
  asinh EmptyInterval   = EmptyInterval
  asinh InvalidInterval = InvalidInterval 
  asinh k = increasing asinh k
  {-# INLINE asinh #-}
  acosh EmptyInterval   = EmptyInterval
  acosh InvalidInterval = InvalidInterval 
  acosh x@(K a b) | a > b = dualize acosh x      
  acosh (K a b)
    | b < 1 = EmptyInterval
    | otherwise = K lo $ acosh b
    where lo | a <= 1 = 0
             | otherwise = acosh a
  {-# INLINE acosh #-}
  atanh EmptyInterval   = EmptyInterval
  atanh InvalidInterval = InvalidInterval 
  atanh x@(K a b) | a > b = dualize atanh x      
  atanh (K a b)
    | b < -1 || a > 1 = EmptyInterval
    | otherwise = K (if a <= - 1 then negInfinity else atanh a) (if b >= 1 then posInfinity else atanh b)
  {-# INLINE atanh #-}
  sqrt EmptyInterval   = EmptyInterval
  sqrt InvalidInterval = InvalidInterval 
  sqrt x@(K a b)
    | a < 0 || b < 0 = EmptyInterval 
    | otherwise      = x ** 0.5
  {-# INLINE sqrt #-}   
  
-- | TODO: implement this instance completly.
-- For now only isNaN and isInfinite functions are useful to me :)
instance (Show a, RealFloat a) => RealFloat (Kaucher a) where
  floatRadix = error "unimplemented"
  floatDigits = error "unimplemented"
  floatRange = error "unimplemented"
  decodeFloat = error "unimplemented"
  encodeFloat = error "unimplemented"
  exponent = error "unimplemented"
  significand = error "unimplemented"
  scaleFloat = error "unimplemented"
  isDenormalized = error "unimplemented"
  isNegativeZero = error "unimplemented"
  atan2 = error "unimplemented"

  isIEEE _ = False
  isNaN EmptyInterval   = False
  isNaN InvalidInterval = True 
  isNaN (K a b) = isNaN a || isNaN b -- this shouldn't happen
  {-# INLINE isNaN #-}
  
  isInfinite EmptyInterval   = False
  isInfinite InvalidInterval = False 
  isInfinite (K a b) = isInfinite a || isInfinite b
  {-# INLINE isInfinite #-}

instance (Ord a, RealFloat a) => OptIntPow (Kaucher a) where
  (^.) = (|^^|)
  {-# INLINE (^.) #-}

