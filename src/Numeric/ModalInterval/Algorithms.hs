{-# LANGUAGE TupleSections #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.ModalInterval.Algorithms
-- Copyright   :  (c) 2021 Fabrício Olivetti
-- License     :  BSD3
-- Maintainer  :  fabricio.olivetti@gmail.com
-- Stability   :  experimental
-- Portability :  TupleSections
--
-- Kaucher Interval arithmetic algorithms for outer and inner approximations
-- based on https://tel.archives-ouvertes.fr/tel-00331668/document
--
-----------------------------------------------------------------------------
module Numeric.ModalInterval.Algorithms 
    ( outerApprox
    , innerApprox
    , meanValue
    , evalKaucher
    )
    where

import Numeric.ModalInterval

import Control.Monad.State 
import Data.Map.Strict hiding (map, singleton, empty, foldr, filter)
import qualified Data.Map.Strict as M
import Data.SRTree
import Data.Maybe (fromMaybe)
import Data.SRTree (deriveBy, relabelOccurrences, getChildren)

type Domains = Map Int (Kaucher Double)
type KDomains = Map (Int, Int) (Kaucher Double) -- the second element is the incidence counter

type KTree   = SRTree (Int, Int) (Kaucher Double) -- tree for modal arithmetic with incidences
type Tree    = SRTree Int (Kaucher Double)


-- | Outer approximation of an expression tree given the domains of the variables.
-- To calculate the outer approximation we need to apply "dualizer" to every
-- variable that is totally monotonic.
--
-- See page 61 of https://tel.archives-ouvertes.fr/tel-00331668/document (Eq. 3.80)
outerApprox :: Tree -> Domains -> Kaucher Double 
outerApprox t domains = outF t kt domains kdomains 
    where (kt, kdomains) = mkKTree t domains 

-- | Inner approximation of an expression tree given the domains of the variables.
-- To calculate the outer approximation we need to apply "dualizer" to every
-- variable that is totally monotonic and replace those non-monotonic with the midpoint 
-- of the domain.
--
-- See page 61 of https://tel.archives-ouvertes.fr/tel-00331668/document (Eq. 3.79)
innerApprox :: Tree -> Domains -> Kaucher Double  
innerApprox t domains = innF t kt domains kdomains 
    where (kt, kdomains) = mkKTree t domains 

-- | In Modal Arithmetic we have proper Kauchers [a, b] with a <= b
-- and improper Kauchers [a, b] with a > b.

-- A constraint f(x, y) \subset [a, b] means:
--
-- if both x, y are proper: for any value of x, y inside their domains, the range of f(x,y) is within [a, b]
-- if x is proper and y is improper: for any value of x, it exists a value of y which f(x,y) evaluates within [a, b]
-- if x, y are improper: there exists a value of x and a value of y which f(x,y) evaluates within [a, b]
evalModal :: KTree -> KDomains -> Kaucher Double
evalModal kt = fromMaybe empty . evalTreeWithMap kt

evalKaucher :: Tree -> Domains -> Kaucher Double
evalKaucher t = fromMaybe empty . evalTreeWithMap t

-- | Outer approximation of an expression tree given the domains of the variables.
-- To calculate the outer approximation we need to apply "dualizer" to every
-- variable that is totally monotonic.
--
-- See page 61 of https://tel.archives-ouvertes.fr/tel-00331668/document (Eq. 3.80)
outF :: Tree -> KTree -> Domains -> KDomains -> Kaucher Double
outF t kt domains kdomains = evalModal kt 
                           $ foldr (flip (dualizer t kt domains)) kdomains 
                           $ keys domains

-- | Inner approximation of an expression tree given the domains of the variables.
-- To calculate the outer approximation we need to apply "dualizer" to every
-- variable that is totally monotonic and replace those non-monotonic with the midpoint 
-- of the domain.
--
-- See page 61 of https://tel.archives-ouvertes.fr/tel-00331668/document (Eq. 3.79)
innF :: Tree -> KTree -> Domains -> KDomains -> Kaucher Double
innF t kt domains kdomains = evalModal kt 
                           $ foldr (flip (pointDual t kt domains)) kdomains
                           $ keys domains

pointDual, dualizer, pointify :: Tree -> KTree -> Domains -> KDomains -> Int -> KDomains

-- | Apply dualizer to those variables that are totally monotonic and 
-- pointify to those non-monotonic, do nothing if the variable has only one incidence.
pointDual t kt domains kdomains ix
  | occurrencesOf ix kdomains == 1          = kdomains
  | isTotMonotonic t kt domains kdomains ix = kdomainsDual
  | otherwise                               = kdomainsPoint
  where
    kdomainsPoint = mapOnKey ix (singleton . midpoint) kdomains
    sign          = monoSign $ evalKaucher (deriveBy ix t) domains
    kdomainsDual  = mapWithKey (\k v -> if fst k==ix 
                                           then (toDual . incSign) k v 
                                           else v) kdomains 
    incSign       = monoSign . (`evalModal` kdomains) . (`deriveBy` kt)
    toDual s  | s == sign = id
              | otherwise = dual
{-# INLINE pointDual #-}

-- | Apply dualizer to the totally monotonic variables, do nothing if the variable has only one incidence.
-- For every monotonic variable, it replaces the domain of that incidence by its dual if the direction
-- of monotonicty is opposite to the monotonicity of the variable.
--
-- E.g., if xi is monotonically increasing and xij is monotonically decreasing, it replaces the domain 
-- of xij by its dual.
dualizer t kt domains kdomains ix
  | occurrencesOf ix kdomains == 1  = kdomains
  | varMonotonic && allMonotonic    = kdomains'
  | otherwise                       = kdomains
  where
    t'        = evalKaucher (deriveBy ix t) domains
    kt'       = mapWithKey (\k _ -> evalModal (deriveBy k kt) kdomains) 
              $ M.filterWithKey (\k _ -> fst k ==ix) kdomains
    varMonotonic = isMonotonic t'
    allMonotonic = all isMonotonic $ map snd $ M.toList kt'
    sign      = monoSign t'
    kdomains' = mapWithKey (\ k v -> if fst k == ix 
                                        then (toDual . monoSign) (kt' ! k) v 
                                        else v) kdomains
    --incSign   = monoSign . (`evalModal` kdomains) . (`deriveBy` kt)
    toDual s  | s == sign = id
              | otherwise = dual
{-# INLINE dualizer #-}

-- | Apply pointify (midpoint of the domain) to the non-monotonic variables, do nothing if the variable has only one incidence.
pointify t kt domains kdomains ix
  | occurrencesOf ix kdomains == 1          = kdomains
  | isTotMonotonic t kt domains kdomains ix = kdomains
  | otherwise                               = kdomains'
  where
    kdomains' = mapOnKey ix (singleton . midpoint) kdomains
{-# INLINE pointify #-}

occurrencesOf :: Int -> KDomains -> Int
occurrencesOf ix = size . M.filterWithKey (\k _ -> fst k == ix)
{-# INLINE occurrencesOf #-}

mapOnKey :: Int -> (Kaucher Double -> Kaucher Double) -> KDomains -> KDomains
mapOnKey ix f = M.mapWithKey (\k v -> if fst k == ix then f v else v)
{-# INLINE mapOnKey #-}

-- | Check if a variable is totally monotonic.
--
-- A variable xi is totally monotonic if:
-- * xi is monotonic within the domain
-- * every xij is monotonic within the domain
-- xij is the j-th occurrence of the i-th variable in the expression
isTotMonotonic :: Tree -> KTree -> Domains -> KDomains -> Int -> Bool
isTotMonotonic t kt domains kdomains ix = occurrenceMonotonic && varMonotonic
  where
    varMonotonic        = isMonotonic
                        $ evalKaucher (deriveBy ix t) domains
    iys                 = map fst $ M.toList $ M.filterWithKey (\k _ -> fst k ==ix) kdomains -- map fst $ toList ixs
    occurrenceMonotonic = all checkMonotonicity iys
    checkMonotonicity   = isMonotonic . (`evalModal` kdomains) . (`deriveBy` kt)
{-# INLINE isTotMonotonic #-}

-- | Calculate the sign of the monotonicity
-- -1 means decreasing, 0 means non-monotonic, 1 means increasing
monoSign :: Kaucher Double -> Int
monoSign img
  | img `isSubsetOf` (0 <.< 1/0)  = 1
  | img `isSubsetOf` (-1/0 <.< 0) = -1
  | otherwise                     = 0
{-# INLINE monoSign #-}

-- | Given the image of the partial derivative of an expression w.r.t. a variable,
-- returns whether it is monotonic or not.
isMonotonic :: Kaucher Double -> Bool
isMonotonic img = img `isSubsetOf` (0 <.< 1/0) || img `isSubsetOf` (-1/0 <.< 0)
{-# INLINE isMonotonic #-}

mkKTree :: Tree -> Domains -> (KTree, KDomains) 
mkKTree t domains = (kt, kdomains)
  where 
    getVar :: (Int, Int) -> State [(Int, Int)] ()
    getVar ix = do s <- get 
                   put $ s <> [ix] 
                   pure () 
    kt              = relabelOccurrences t
    kdomains        = fromList [(k, domains ! fst k) | k <- traverseIx getVar kt `execState` [] ]
{-# INLINE mkKTree #-}

meanValue :: Tree -> Domains -> Kaucher Double
meanValue expr domain = innerApprox expr domain + sum [derivTerms ix | ix <- [0..dim-1]]
  where
    derivTerms ix -- | isMonotonic $ evalKaucher (deriveBy ix expr) domain = singleton 0.0
                  | otherwise = outerApprox (deriveBy ix expr) (M.map proper domain) * (domain M.! ix - domain' M.! ix)
    dim = M.size domain
    mid = singleton . midpoint
    domain' = M.map mid domain 
{-# INLINE meanValue #-}
