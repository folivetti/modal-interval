import           Test.Tasty
import           Test.Tasty.HUnit
import           Numeric.ModalInterval

x, y, z :: Kaucher Double
x = 0 <.< 1
y = 0 <.< 1
z = 1 <.< 0

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
    , testCase "[0, 1] + [0, 1] * [0, 1]" (assertEqual "" (0 <.< 2) (x + x * y))
    , testCase "[0, 1] + [1, 0] * [0, 1]" (assertEqual "" (0 <.< 1) (x + z * y))
    , testCase "2*x*y - x*y + cos(z), -1 < x < 1, 1 < y < 2, -10 < z < 10" (assertEqual "" (-7 <.< 7) cosineTest)
    , testCase "exp((-(x1/x0)^2)/2)/(sqrt(2*pi)*x0)), 1 < x0, x1 < 3" (assertEqual "" (1.4772828039793357e-3 <.< 0.3773832276929932) expSqrtTest)
    ]
    
main :: IO ()
main = defaultMain tests
