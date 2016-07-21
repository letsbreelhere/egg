module Main where

import qualified Compiler

main :: IO ()
main = either print putStrLn =<< Compiler.compile =<< getContents
