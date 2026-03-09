module HaskProto.Value (
  Value (..),
  lookupField,
  hasField,
  messageFields,
  repeatedValues,
)
where

import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64, Word8)

-- Value
-- Dynamic value representation. Since we can't generate
-- Haskell types at runtime from schemas, we use this
-- universal value type instead.
data Value
  = VInt32 Int32
  | VInt64 Int64
  | VUInt32 Word32
  | VUInt64 Word64
  | VBool Bool
  | VString String
  | VBytes [Word8]
  | VDouble Double
  | VMessage [(String, Value)]
  | VRepeated [Value]
  | VNull
  deriving (Show, Eq)

-- lookupField
-- Look up a field by name in a VMessage value.
-- Returns Nothing for non-message values or missing fields.
lookupField :: String -> Value -> Maybe Value
lookupField name (VMessage fields) =
  case filter (\(n, _) -> n == name) fields of
    [(_, val)] -> Just val
    _ -> Nothing
lookupField _ _ = Nothing

-- hasField
-- Check whether a VMessage has a given field name.
hasField :: String -> Value -> Bool
hasField name val =
  case lookupField name val of
    Just _ -> True
    Nothing -> False

-- messageFields
-- Extract the field list from a VMessage.
messageFields :: Value -> [(String, Value)]
messageFields (VMessage fields) = fields
messageFields _ = []

-- repeatedValues
-- Extract the list from a VRepeated.
repeatedValues :: Value -> [Value]
repeatedValues (VRepeated vs) = vs
repeatedValues _ = []
