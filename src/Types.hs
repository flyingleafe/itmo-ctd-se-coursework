module Types
       ( Base
       , TMError
       , runBase
       ) where

import           Control.Monad.Except (ExceptT, runExceptT)
import           Universum

type Base e = ExceptT e IO
type TMError = Text

runBase :: Base e a -> IO (Either e a)
runBase = runExceptT
