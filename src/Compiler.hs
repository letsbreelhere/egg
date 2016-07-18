module Compiler where

import qualified Codegen
import qualified Lexer
import qualified Parser
import Data.Bifunctor (first)
import Text.Megaparsec.Error (ParseError)
import Control.Exception.Base (try, SomeException)

data CompilationError = ParseError ParseError
                      | LlvmError Codegen.LlvmError
                      | CheckerError String
                      | OtherError SomeException

instance Show CompilationError where
  show (ParseError p)   = "Parse error: " ++ show p
  show (LlvmError l)    = "Assembly error: " ++ show l
  show (CheckerError s) = show s
  show (OtherError e)   = "Exception before assembly: " ++ show e

compile :: String -> IO (Either CompilationError String)
compile input =
  let parsed = first ParseError $ Parser.parse "" =<< Lexer.lex "" input
  in case parsed of
    Left err      -> pure (Left err)
    Right funDefs -> do tried <- (try $ Codegen.toAssembly funDefs) :: IO (Either SomeException (Either Codegen.LlvmError String))
                        case tried of
                          Left err -> pure . Left . OtherError $ err
                          Right assembled -> pure $ first LlvmError assembled
