module Test.Compiler where

import qualified Parser
import qualified Lexer
import qualified Compiler
import Test.Tasty
import Test.Tasty.HUnit
import GHC.IO.Handle (hPutStr, hClose)
import System.Exit (ExitCode(..))
import qualified System.Process as Proc

compilerSpec :: TestTree
compilerSpec = testGroup "Code generation and compiler"
  [ testCase "simple return values" $
      testInterpreterFile "return42.egg" (ExitFailure 42)
  , testCase "arithmetic" $
      testInterpreterFile "arithmetic.egg" (ExitFailure 42)
  , testCase "global function calls" $
      testInterpreterFile "function_calls.egg" (ExitFailure 42)
  , testCase "factorial" $
      testInterpreterFile "fact.egg" (ExitFailure 42)
  , testCase "lambdas" $
      testInterpreterFile "lam.egg" (ExitFailure 42)
  , testCase "closures" $
      testInterpreterFile "closure.egg" (ExitFailure 42)
  ]

compile :: String -> IO String
compile input = do
  c <- Compiler.compile input
  case c of
    Left err -> do
      assertFailure (show err)
      undefined -- Gotta make the compiler happy, but this is unreachable
    Right asm -> pure asm

testInterpreterFile :: FilePath -> ExitCode -> Assertion
testInterpreterFile path code = do
  input <- readFile ("test/resources/" ++ path)
  testInterpreter input code

testInterpreter :: String -> ExitCode -> Assertion
testInterpreter input expectedCode = do
  assembly <- compile input
  (Just ihdl, _, _, ph) <- Proc.createProcess (Proc.proc "lli-3.5" []){Proc.std_in = Proc.CreatePipe}
  hPutStr ihdl assembly >> hClose ihdl
  exitCode <- Proc.waitForProcess ph
  exitCode @?= expectedCode
