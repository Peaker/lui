{-# OPTIONS_GHC -Wall -O2
  #-}

module Func where

import Prelude hiding ((.))
import Control.Category(Category, (.))

result :: Category cat => cat b c -> cat a b -> cat a c
result = (.)

argument :: Category cat => cat a b -> cat b c -> cat a c
argument = flip (.)

infixr 2 ~>
(~>) :: (Category cat) => cat a b1 -> cat b c -> cat b1 b -> cat a c
(arg ~> res) func = res . func . arg
