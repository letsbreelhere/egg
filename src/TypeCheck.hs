module TypeCheck where

import           Control.Cofree
import           Types.EType
import           Types.Expr
import           Types.Constant
import           Supply
import           Data.Map (Map)
import           Unification

data AnnState =
       AnnState
         { _tyVars :: Supply Int
         , _equations :: [Equation EType]
         , _symtab :: Map String EType
         }

constantType :: Constant -> EType
constantType c =
  case c of
    I _  -> Ty "int"
    C _  -> Ty "char"
    B _  -> Ty "bool"
    Unit -> Ty "void"
