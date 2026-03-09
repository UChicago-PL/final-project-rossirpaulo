module HaskProto.Pretty (prettySchema, prettyMessage, prettyField) where

import HaskProto.Schema

-- prettySchema
-- Pretty-print an entire schema.
prettySchema :: Schema -> String
prettySchema (Schema msgs) =
  unlines (concatMap (\m -> prettyMessage m ++ [""]) msgs)

-- prettyMessage
-- Pretty-print a single message definition.
prettyMessage :: MessageDef -> [String]
prettyMessage msg =
  ["message " ++ msgName msg ++ " {"]
    ++ map (\f -> "  " ++ prettyField f) (msgFields msg)
    ++ ["}"]

-- prettyField
-- Pretty-print a single field definition.
prettyField :: FieldDef -> String
prettyField field =
  fieldName field
    ++ ": "
    ++ prettyModifier (fieldModifier field)
    ++ prettyType (fieldType field)
    ++ " @"
    ++ show (fieldTag field)

-- prettyModifier
prettyModifier :: Modifier -> String
prettyModifier Required = ""
prettyModifier Optional = "optional "
prettyModifier Repeated = "repeated "

-- prettyType
prettyType :: FieldType -> String
prettyType (PrimType prim) = prettyPrim prim
prettyType (RefType name) = name

-- prettyPrim
prettyPrim :: PrimTy -> String
prettyPrim TInt32 = "int32"
prettyPrim TInt64 = "int64"
prettyPrim TUInt32 = "uint32"
prettyPrim TUInt64 = "uint64"
prettyPrim TBool = "bool"
prettyPrim TString = "string"
prettyPrim TBytes = "bytes"
prettyPrim TDouble = "double"
