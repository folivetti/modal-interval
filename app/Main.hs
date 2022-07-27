module Main where

import           Numeric.ModalInterval
import           Numeric.ModalInterval.Algorithms
import           Data.SRTree 
import qualified Data.Map.Strict as M

x, y, z :: Kaucher Double
x = 0 <.< 1
y = 0 <.< 1
z = 1 <.< 0

x0 = Var 0
x1 = Var 1
x2 = Var 2

domains1 = M.fromList [(0, 0 <.< 1), (1, 0 <.< 1)]
domains2 = M.fromList [(0, -1 <.< 1), (1, 1 <.< 2), (2, -10 <.< 10)]
domains3 = M.fromList [(0, 1 <.< 3), (1, 1 <.< 3)]
domains4 = M.fromList [(0, -1 <.< 5), (2, 1 <.< 2), (1, 1 <.< 10)]
domains5 = M.fromList [(0, -1 <.< 2.5), (1, 1 <.< 5)]
domains6 = M.fromList [(0, 2.5 <.< 5), (1, 5 <.< 5)]
domains7 = M.fromList [(0, 2.5 <.< 2.5), (1, 1 <.< 5)]
domains8 = M.fromList [(0, 2.5 <.< 2.5), (1, 5 <.< 10)]

expr1 = x0 + x0 * x1
expr2 = 2 * x0 * x1 - x0 * x1 + cos x2
expr3 = exp((-(x1/x0)^2)/2)/(sqrt(2*pi)*x0)

expr4 :: SRTree Int (Kaucher Double)
expr4 =  x0^2 + x0*x1

cosineTest :: Kaucher Double
cosineTest = 2 * (-1 <.< 1) * (1 <.< 2) - (-1 <.< 1) * (1 <.< 2) + cos (-10 <.< 10)

expSqrtTest :: Kaucher Double
expSqrtTest = exp((-(x1/x0)^2)/2)/(sqrt(2*pi)*x0)
  where 
    x0 = 1 <.< 3
    x1 = 1 <.< 3

main :: IO ()
main = do 
    --print $ innerApprox expr1 domains1 
    --print $ outerApprox expr1 domains1 
    --print $ innerApprox expr2 domains2 
    --print $ outerApprox expr2 domains2 
    --print $ innerApprox expr3 domains3 
    --print $ outerApprox expr3 domains3 
    print $ innerApprox expr4 domains4 
    print $ outerApprox expr4 domains4 
    print $ innerApprox expr4 domains5 
    print $ outerApprox expr4 domains5 
