module HaskProto.Json
  ( parseJson
  , renderJson
  , valueToJson
  , jsonToValue
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Map as M
import qualified Data.Scientific as Sci
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word (Word8)

import HaskProto.Schema
import HaskProto.Value

-- parseJson
-- Parse a JSON string using aeson.
parseJson :: String -> Maybe Aeson.Value
parseJson = Aeson.decode . BLC.pack

-- renderJson
-- Render an aeson Value as compact JSON text.
renderJson :: Aeson.Value -> String
renderJson = BLC.unpack . Aeson.encode

-- valueToJson
-- Convert a Value to an aeson Value.
valueToJson :: Value -> Aeson.Value
valueToJson (VInt32 n)     = Aeson.Number (fromIntegral n)
valueToJson (VInt64 n)     = Aeson.Number (fromIntegral n)
valueToJson (VUInt32 n)    = Aeson.Number (fromIntegral n)
valueToJson (VUInt64 n)    = Aeson.Number (fromIntegral n)
valueToJson (VBool b)      = Aeson.Bool b
valueToJson (VString s)    = Aeson.String (T.pack s)
valueToJson (VBytes bs)    = Aeson.Array (V.fromList (map (Aeson.Number . fromIntegral) bs))
valueToJson (VDouble d)    = Aeson.Number (Sci.fromFloatDigits d)
valueToJson (VMessage fs)  =
  Aeson.Object (KM.fromList [(Key.fromString k, valueToJson v) | (k, v) <- fs])
valueToJson (VRepeated vs) = Aeson.Array (V.fromList (map valueToJson vs))
valueToJson VNull          = Aeson.Null

-- jsonToValue
-- Convert an aeson Value to a Value using the schema to
-- determine the correct types for each field.
jsonToValue :: SchemaCtx -> String -> Aeson.Value -> Either String Value
jsonToValue ctx name (Aeson.Object obj) =
  case M.lookup name ctx of
    Nothing     -> Left ("unknown message: " ++ name)
    Just msgDef -> do
      fields <- mapM (convertField ctx obj) (msgFields msgDef)
      return (VMessage fields)
jsonToValue _ _ _ = Left "expected a JSON object for a message"

-- convertField
convertField :: SchemaCtx -> Aeson.Object -> FieldDef
             -> Either String (String, Value)
convertField ctx obj fd =
  let fname = fieldName fd
      key   = Key.fromString fname
  in case (fieldModifier fd, KM.lookup key obj) of
       (Required, Nothing)  ->
         Left ("missing required field: " ++ fname)
       (Required, Just jv)  -> do
         v <- convertTyped ctx (fieldType fd) jv
         return (fname, v)
       (Optional, Nothing)         -> return (fname, VNull)
       (Optional, Just Aeson.Null) -> return (fname, VNull)
       (Optional, Just jv)         -> do
         v <- convertTyped ctx (fieldType fd) jv
         return (fname, v)
       (Repeated, Nothing)  -> return (fname, VRepeated [])
       (Repeated, Just (Aeson.Array arr)) -> do
         vs <- mapM (convertTyped ctx (fieldType fd)) (V.toList arr)
         return (fname, VRepeated vs)
       (Repeated, Just _)   ->
         Left ("expected JSON array for repeated field: " ++ fname)

-- convertTyped
-- Convert an aeson Value to a Value given the expected field type.
convertTyped :: SchemaCtx -> FieldType -> Aeson.Value -> Either String Value
convertTyped _ (PrimType TInt32)  (Aeson.Number n) = Right (VInt32 (round n))
convertTyped _ (PrimType TInt64)  (Aeson.Number n) = Right (VInt64 (round n))
convertTyped _ (PrimType TUInt32) (Aeson.Number n) = Right (VUInt32 (round n))
convertTyped _ (PrimType TUInt64) (Aeson.Number n) = Right (VUInt64 (round n))
convertTyped _ (PrimType TBool)   (Aeson.Bool b)   = Right (VBool b)
convertTyped _ (PrimType TDouble) (Aeson.Number n) = Right (VDouble (Sci.toRealFloat n))
convertTyped _ (PrimType TString) (Aeson.String s) = Right (VString (T.unpack s))
convertTyped _ (PrimType TBytes) (Aeson.Array ns) =
  VBytes <$> mapM toByte (V.toList ns)
  where
    toByte (Aeson.Number n) = Right (round n :: Word8)
    toByte _                = Left "expected number in bytes array"
convertTyped ctx (RefType name) jv = jsonToValue ctx name jv
convertTyped _ ft jv =
  Left ("type mismatch: expected " ++ showFt ft
        ++ " but got JSON " ++ showJt jv)
  where
    showFt (PrimType p) = show p
    showFt (RefType r)  = r
    showJt (Aeson.Object _) = "object"
    showJt (Aeson.Array _)  = "array"
    showJt (Aeson.String _) = "string"
    showJt (Aeson.Number _) = "number"
    showJt (Aeson.Bool _)   = "bool"
    showJt Aeson.Null       = "null"
