module Util where

import Control.Monad.Trans.Cont
import RIO

(??) :: Maybe a -> m r -> ContT r m a
Nothing ?? m = ContT $ const m
Just a ?? _ = pure a

(???) :: Either e a -> (e -> m r) -> ContT r m a
Left e ??? k = ContT $ const $ k e
Right a ??? _ = pure a
