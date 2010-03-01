{-# OPTIONS_GHC -Wall -O2 #-}

module Data.Editor.Function(result, argument, (~>)) where

-- TODO: Put this in a more generic library

import Control.Category(Category, (<<<), (>>>))

result :: Category cat => cat b c -> cat a b -> cat a c
result = (<<<)

argument :: Category cat => cat a b -> cat b c -> cat a c
argument = (>>>)

infixr 2 ~>
(~>) :: (Category cat) => cat a b -> cat c d -> cat b c -> cat a d
(arg ~> res) func = arg >>> func >>> res
