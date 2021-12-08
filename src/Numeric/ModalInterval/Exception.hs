module Numeric.ModalInterval.Exception 
  ( EmptyIntervalError(..)
    , InvalidIntervalError(..)
  ) where 

import Control.Exception 

data EmptyIntervalError = EmptyIntervalError String deriving (Eq, Ord)

instance Show EmptyIntervalError where
    show (EmptyIntervalError f) = "error: partial function applied to empty interval in " <> show f

instance Exception EmptyIntervalError

data InvalidIntervalError = InvalidIntervalError String deriving (Eq, Ord) 

instance Show InvalidIntervalError where
    show (InvalidIntervalError f) = "error: partial function applied to NaN interval in " <> show f

instance Exception  InvalidIntervalError
