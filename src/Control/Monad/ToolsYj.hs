{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.ToolsYj where

import Control.Monad.Fix
import Data.Bool

whenDef :: Applicative m => a -> Bool -> m a -> m a
whenDef = flip . bool . pure

whenMaybe :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenMaybe = flip . maybe $ pure ()

whenMaybeDef :: Applicative m => b -> Maybe a -> (a -> m b) -> m b
whenMaybeDef = flip . maybe . pure

doWhile_ :: Monad m => m Bool -> m ()
doWhile_ a = fix $ (a >>=) . bool (pure ())
