{-# OPTIONS_GHC -Wall -O2
  #-}

module MyMonad where
    import Control.Monad(liftM)
    whileM :: Monad m => (a -> Bool) -> m a -> m [a]
    whileM cond element = do
      value <- element
      if cond value
        then liftM (value:) (whileM cond element)
        else return []
