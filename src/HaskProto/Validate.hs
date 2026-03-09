module HaskProto.Validate (validateSchema, ValidationError (..)) where

import qualified Data.Map as M
import qualified Data.Set as S
import HaskProto.Schema

-- ValidationError
-- Validation errors that can be reported.
data ValidationError
  = DuplicateMessageName String
  | DuplicateFieldName String String
  | DuplicateTag String Int
  | UnresolvedTypeRef String String String
  | InvalidTag String String Int
  deriving (Show, Eq)

-- validateSchema
-- Validate a schema, returning a list of all errors found.
-- An empty list means the schema is valid.
validateSchema :: Schema -> [ValidationError]
validateSchema (Schema msgs) =
  let messageNames = S.fromList (map msgName msgs)
   in checkDuplicateMessages msgs
        ++ concatMap (checkMessage messageNames) msgs

-- checkDuplicateMessages
checkDuplicateMessages :: [MessageDef] -> [ValidationError]
checkDuplicateMessages msgs =
  let names = map msgName msgs
      duplicates = findDuplicates names
   in map DuplicateMessageName duplicates

-- checkMessage
-- Check a single message for field-level issues.
checkMessage :: S.Set String -> MessageDef -> [ValidationError]
checkMessage messageNames msg =
  let name = msgName msg
      fields = msgFields msg
   in checkDuplicateFieldNames name fields
        ++ checkDuplicateTags name fields
        ++ checkFieldTypes messageNames name fields
        ++ checkValidTags name fields

-- checkDuplicateFieldNames
checkDuplicateFieldNames :: String -> [FieldDef] -> [ValidationError]
checkDuplicateFieldNames msgN fields =
  let names = map fieldName fields
      duplicates = findDuplicates names
   in map (DuplicateFieldName msgN) duplicates

-- checkDuplicateTags
checkDuplicateTags :: String -> [FieldDef] -> [ValidationError]
checkDuplicateTags msgN fields =
  let tags = map fieldTag fields
      duplicates = findDuplicates tags
   in map (DuplicateTag msgN) duplicates

-- checkFieldTypes
-- Check that all type references point to defined messages.
checkFieldTypes :: S.Set String -> String -> [FieldDef] -> [ValidationError]
checkFieldTypes messageNames msgN fields =
  concatMap checkField fields
 where
  checkField field =
    case fieldType field of
      RefType refName
        | not (S.member refName messageNames) ->
            [UnresolvedTypeRef msgN (fieldName field) refName]
      _ -> []

-- checkValidTags
-- Check that all tags are positive integers.
checkValidTags :: String -> [FieldDef] -> [ValidationError]
checkValidTags msgN fields =
  concatMap checkField fields
 where
  checkField field =
    if fieldTag field <= 0
      then [InvalidTag msgN (fieldName field) (fieldTag field)]
      else []

-- findDuplicates
findDuplicates :: (Ord a) => [a] -> [a]
findDuplicates xs =
  let counts = M.fromListWith (+) [(x, 1 :: Int) | x <- xs]
   in M.keys (M.filter (> 1) counts)
