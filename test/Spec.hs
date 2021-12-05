import           Test.Tasty
import           Test.Tasty.HUnit
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

expr1 = x0 * x1 - x0
expr2 = 2 * x0 * x1 - x0 * x1 + cos x2

cosineTest :: Kaucher Double
cosineTest = 2 * (-1 <.< 1) * (1 <.< 2) - (-1 <.< 1) * (1 <.< 2) + cos (-10 <.< 10)

expSqrtTest :: Kaucher Double
expSqrtTest = exp((-(x1/x0)^2)/2)/(sqrt(2*pi)*x0)
  where 
    x0 = 1 <.< 3
    x1 = 1 <.< 3

tests :: TestTree
tests =
  testGroup
    "Operations test"
    [ testCase "[0, 1] + [0, 1]" (assertEqual "" (0 <.< 2) (x + y))
    , testCase "[0, 1] * [0, 1]" (assertEqual "" (0 <.< 1) (x * y))
    , testCase "[0, 1] * [0, 1] - [0, 1]" (assertEqual "" (-1 <.< 1) ( x * y - x))
    , testCase "[1, 0] * [0, 1] - [0, 1]" (assertEqual "" (-1 <.< 0) (z * y - x))
    , testCase "2*x*y - x*y + cos(z), -1 < x < 1, 1 < y < 2, -10 < z < 10" (assertEqual "" (-7 <.< 7) cosineTest)
    , testCase "exp((-(x1/x0)^2)/2)/(sqrt(2*pi)*x0)), 1 < x0, x1 < 3" (assertEqual "" (1.4772828039793357e-3 <.< 0.3773832276929932) expSqrtTest)
    , testCase "x0*x1 - x0, 0 < x0, x1 < 1" (assertEqual "" (-1 <.< 0) (innerApprox expr1 domains1))
    , testCase "x0*x1 - x0, 0 < x0, x1 < 1" (assertEqual "" (-1 <.< 0) (outerApprox expr1 domains1))
    , testCase "2 * x0 * x1 - x0 * x1 + cos x2, -1 < x0 < 1, 1 < x1 < 2, -10 < x2 < 10" (assertEqual "" (-2.5 <.< 2.5) (innerApprox expr2 domains2))
    , testCase "2 * x0 * x1 - x0 * x1 + cos x2, -1 < x0 < 1, 1 < x1 < 2, -10 < x2 < 10" (assertEqual "" (-4 <.< 4) (outerApprox expr2 domains2))
    ]
    
main :: IO ()
main = defaultMain tests
