module Compiler where

import qualified Codegen
import qualified Lexer
import qualified Parser
import Text.Megaparsec.Error (ParseError)

compile :: String -> IO (Either ParseError String)
compile input = sequenceA . fmap Codegen.toAssembly $ Parser.parse "" =<< Lexer.lex "" input
