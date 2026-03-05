module HaskProto.Schema where

-- schema
-- A Schema is a collection of message definitions.
data Schema = Schema [MessageDef]
  deriving (Show, Eq)

-- messageDef
-- A message has a name and a list of fields.
data MessageDef = MessageDef
  { msgName :: String,
    msgFields :: [FieldDef]
  }
  deriving (Show, Eq)

-- fieldDef
-- A field has a name, type, modifier, and numeric tag.
data FieldDef = FieldDef
  { fieldName :: String,
    fieldType :: FieldType,
    fieldModifier :: Modifier,
    fieldTag :: Int
  }
  deriving (Show, Eq)

-- fieldType
-- Field types: either a primitive or a reference to another
-- message by name.
data FieldType
  = PrimType PrimTy
  | RefType String
  deriving (Show, Eq)

-- primTy
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

-- modifier
-- Field modifiers.
data Modifier
  = Required
  | Optional
  | Repeated
  deriving (Show, Eq, Ord)
