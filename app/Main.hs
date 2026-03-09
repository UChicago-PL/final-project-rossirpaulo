module Main where

import qualified Data.ByteString as BS
import System.Environment (getArgs)
import System.Exit (die)

import HaskProto.Json (jsonToValue, parseJson, renderJson, valueToJson)
import HaskProto.Parser (parseSchema)
import HaskProto.Pretty (prettySchema)
import HaskProto.Schema (Schema, buildContext)
import HaskProto.Validate (ValidationError, validateSchema)
import HaskProto.Wire (decodeMessage, encodeMessage)

-- main
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["validate", path] ->
      runValidate path
    ["encode", schemaPath, msgName, jsonPath, outPath] ->
      runEncode schemaPath msgName jsonPath outPath
    ["decode", schemaPath, msgName, binPath] ->
      runDecode schemaPath msgName binPath
    _ -> die $ unlines
      [ "usage:"
      , "  haskproto validate <schema.hproto>"
      , "  haskproto encode <schema> <Message> <input.json> <output.bin>"
      , "  haskproto decode <schema> <Message> <input.bin>"
      ]

-- runValidate
-- Parse, check for errors, and pretty-print.
runValidate :: FilePath -> IO ()
runValidate path = do
  schema <- loadSchema path
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

-- runEncode
-- Read JSON, convert to Value, write binary.
runEncode :: FilePath -> String -> FilePath -> FilePath -> IO ()
runEncode schemaPath msgName jsonPath outPath = do
  schema <- loadSchema schemaPath
  jsonStr <- readFile jsonPath
  case parseJson jsonStr of
    Nothing -> die "error: could not parse JSON input"
    Just jval -> do
      let ctx = buildContext schema
      case jsonToValue ctx msgName jval of
        Left err -> die ("json error: " ++ err)
        Right val ->
          case encodeMessage schema msgName val of
            Left err   -> die ("encode error: " ++ show err)
            Right bytes -> BS.writeFile outPath (BS.pack bytes)

-- runDecode
-- Read binary, decode to Value, print JSON.
runDecode :: FilePath -> String -> FilePath -> IO ()
runDecode schemaPath msgName binPath = do
  schema <- loadSchema schemaPath
  bytes <- BS.unpack <$> BS.readFile binPath
  case decodeMessage schema msgName bytes of
    Left err  -> die ("decode error: " ++ show err)
    Right val -> putStrLn (renderJson (valueToJson val))

-- loadSchema
loadSchema :: FilePath -> IO Schema
loadSchema path = do
  contents <- readFile path
  case parseSchema contents of
    Nothing     -> die ("parse error: could not parse " ++ path)
    Just schema -> return schema

-- showError
showError :: ValidationError -> String
showError err = "  - " ++ show err
