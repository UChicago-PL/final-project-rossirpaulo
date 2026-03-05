module Main where

import HaskProto.Parser (parseSchema)
import HaskProto.Pretty (prettySchema)
import HaskProto.Validate (ValidationError, validateSchema)
import HaskProto.Value ()
import System.Environment (getArgs)
import System.Exit (die)

-- MAIN
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["validate", path] -> runValidate path
    _ -> die "usage: haskproto validate <schema.hproto>"

-- runValidate
-- Parse a .hproto file, validate it, and print the result.
runValidate :: FilePath -> IO ()
runValidate path = do
  contents <- readFile path
  case parseSchema contents of
    Nothing -> die ("parse error: could not parse " ++ path)
    Just schema -> do
      let errors = validateSchema schema
      if null errors
        then do
          putStrLn "Schema is valid."
          putStrLn ""
          putStrLn "Pretty-printed:"
          putStr (prettySchema schema)
        else do
          putStrLn "Validation errors:"
          mapM_ (putStrLn . showError) errors

showError :: ValidationError -> String
showError err = "  - " ++ show err
