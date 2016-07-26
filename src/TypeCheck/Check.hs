module TypeCheck.Check where

import           Control.Monad.Writer
import           Types.EType
import           Unification

type CheckerEnv = [(String, EType)]

newtype Check a = Check { runCheck :: Writer [TypeEquation] a }
  deriving (Functor, Applicative, Monad, MonadWriter [TypeEquation], Traversable, Foldable)
