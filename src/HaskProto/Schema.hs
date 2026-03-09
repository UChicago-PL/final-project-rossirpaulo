module HaskProto.Schema where

import qualified Data.Map as M

-- Schema
-- A collection of message definitions.
data Schema = Schema [MessageDef]
  deriving (Show, Eq)

-- MessageDef
-- A message has a name and a list of fields.
data MessageDef = MessageDef
  { msgName :: String,
    msgFields :: [FieldDef]
  }
  deriving (Show, Eq)

-- FieldDef
-- A field has a name, type, modifier, and numeric tag.
data FieldDef = FieldDef
  { fieldName :: String,
    fieldType :: FieldType,
    fieldModifier :: Modifier,
    fieldTag :: Int
  }
  deriving (Show, Eq)

-- FieldType
-- Either a primitive or a reference to another message.
data FieldType
  = PrimType PrimTy
  | RefType String
  deriving (Show, Eq)

-- PrimTy
-- Supported primitive types.
data PrimTy
  = TInt32
  | TInt64
  | TUInt32
  | TUInt64
  | TBool
  | TString
  | TBytes
  | TDouble
  deriving (Show, Eq, Ord)

-- Modifier
-- Field modifiers.
data Modifier
  = Required
  | Optional
  | Repeated
  deriving (Show, Eq, Ord)

-- SchemaCtx
-- Maps message names to their definitions.
-- Shared by Wire and Json modules.
type SchemaCtx = M.Map String MessageDef

-- buildContext
buildContext :: Schema -> SchemaCtx
buildContext (Schema msgs) =
  M.fromList [(msgName m, m) | m <- msgs]
