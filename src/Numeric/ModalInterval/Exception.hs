module Numeric.ModalInterval.Exception 
  ( EmptyIntervalError(..)
    , InvalidIntervalError(..)
  ) where 

import Control.Exception 

data EmptyIntervalError = EmptyIntervalError deriving (Eq, Ord)

instance Show EmptyIntervalError where
    show _ = "error: partial function applied to empty interval"

instance Exception EmptyIntervalError

data InvalidIntervalError = InvalidIntervalError deriving (Eq, Ord) 

instance Show InvalidIntervalError where
    show _ = "error: partial function applied to NaN interval"

instance Exception  InvalidIntervalError
