module HaskProto.Parser (parseSchema) where

import Data.Char (isAlphaNum, isLower, isUpper)
import HaskProto.Schema (
  FieldDef (..),
  FieldType (..),
  MessageDef (..),
  Modifier (..),
  PrimTy (..),
  Schema (..),
 )
import Text.ParserCombinators.ReadP

-- parseSchema
-- Parse a .hproto file into a Schema.
-- Returns Nothing if no complete parse succeeds.
parseSchema :: String -> Maybe Schema
parseSchema input =
  case readP_to_S (schemaP <* eof) input of
    [(schema, "")] -> Just schema
    _ -> Nothing

-- schemaP
schemaP :: ReadP Schema
schemaP = do
  skipSpaces
  msgs <- many1 (messageDef <* skipSpaces)
  return (Schema msgs)

-- messageDef
-- Parse a message definition.
messageDef :: ReadP MessageDef
messageDef = do
  _ <- string "message"
  skipSpaces1
  mname <- parseMessageName
  skipSpaces
  _ <- char '{'
  skipSpaces
  fields <- many (fieldDef <* skipSpaces)
  _ <- char '}'
  return (MessageDef mname fields)

-- fieldDef
-- Parse a field definition.
fieldDef :: ReadP FieldDef
fieldDef = do
  fname <- parseFieldName
  skipSpaces
  _ <- char ':'
  skipSpaces
  modifier <- modifierP
  ftype <- fieldTypeP
  skipSpaces
  tag <- tagP
  return (FieldDef fname ftype modifier tag)

-- modifierP
-- Parse a modifier keyword, or default to Required.
modifierP :: ReadP Modifier
modifierP =
  (string "optional" *> skipSpaces1 *> return Optional)
    <++ (string "repeated" *> skipSpaces1 *> return Repeated)
    <++ return Required

-- fieldTypeP
-- Parse a field type (primitive or message reference).
fieldTypeP :: ReadP FieldType
fieldTypeP =
  (PrimType <$> primTypeP)
    <++ (RefType <$> parseMessageName)

-- primTypeP
-- Parse a primitive type keyword.
primTypeP :: ReadP PrimTy
primTypeP =
  (string "int32" *> return TInt32)
    <++ (string "int64" *> return TInt64)
    <++ (string "uint32" *> return TUInt32)
    <++ (string "uint64" *> return TUInt64)
    <++ (string "bool" *> return TBool)
    <++ (string "string" *> return TString)
    <++ (string "bytes" *> return TBytes)
    <++ (string "double" *> return TDouble)

-- tagP
-- Parse a @n tag annotation.
tagP :: ReadP Int
tagP = do
  _ <- char '@'
  digits <- munch1 (\c -> c >= '0' && c <= '9')
  return (read digits)

-- parseMessageName
-- Uppercase-initial identifier.
parseMessageName :: ReadP String
parseMessageName = do
  first <- satisfy isUpper
  rest <- munch isAlphaNum
  return (first : rest)

-- parseFieldName
-- Lowercase-initial identifier.
parseFieldName :: ReadP String
parseFieldName = do
  first <- satisfy isLower
  rest <- munch (\c -> isAlphaNum c || c == '_')
  return (first : rest)

-- skipSpaces1
-- Skip at least one whitespace character.
skipSpaces1 :: ReadP ()
skipSpaces1 = do
  _ <- satisfy (\c -> c == ' ' || c == '\n' || c == '\t' || c == '\r')
  skipSpaces
