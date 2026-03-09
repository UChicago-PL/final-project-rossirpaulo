module HaskProto.Wire
  ( encodeMessage
  , decodeMessage
  , WireError(..)
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bits ((.&.), (.|.), shiftL, shiftR, xor)
import Data.Int (Int32, Int64)
import Data.Word (Word8, Word64)
import qualified Data.Map as M
import GHC.Float (castDoubleToWord64, castWord64ToDouble)

import HaskProto.Schema
import HaskProto.Value

-- Wire type constants (following Protocol Buffers).
wireTypeVarint :: Int
wireTypeVarint = 0

wireType64Bit :: Int
wireType64Bit = 1

wireTypeLenDel :: Int
wireTypeLenDel = 2

-- WireError
-- Errors that can occur during encoding or decoding.
data WireError
  = UnknownFieldName String String
  | UnknownMessage String
  | TypeMismatch String
  | TruncatedInput
  | InvalidWireType Int
  deriving (Show, Eq)

-- EncodeM
-- Accumulates output bytes in state, reads the schema
-- context, and can throw errors.
type EncodeM a = StateT [Word8] (ReaderT SchemaCtx (Except WireError)) a

-- DecodeM
-- Consumes input bytes from state, reads the schema
-- context, and can throw errors.
type DecodeM a = StateT [Word8] (ReaderT SchemaCtx (Except WireError)) a

-- encodeMessage
encodeMessage :: Schema -> String -> Value -> Either WireError [Word8]
encodeMessage schema name val =
  let ctx = buildContext schema
  in runExcept (runReaderT (execStateT (encodeMsg name val) []) ctx)

-- decodeMessage
decodeMessage :: Schema -> String -> [Word8] -> Either WireError Value
decodeMessage schema name bytes =
  let ctx = buildContext schema
  in runExcept (runReaderT (evalStateT (decodeMsg name) bytes) ctx)

-- encodeVarint
-- Unsigned variable-length encoding, 7 bits per byte, MSB = more.
encodeVarint :: Word64 -> [Word8]
encodeVarint n
  | n < 128   = [fromIntegral n]
  | otherwise =
      let lo = fromIntegral (n .&. 0x7f) .|. 0x80
      in lo : encodeVarint (shiftR n 7)

-- decodeVarintM
decodeVarintM :: DecodeM Word64
decodeVarintM = go 0 0
  where
    go shift acc = do
      b <- consumeByte
      let val = acc .|. (fromIntegral (b .&. 0x7f) `shiftL` shift)
      if b .&. 0x80 == 0
        then return val
        else go (shift + 7) val

-- consumeByte
consumeByte :: DecodeM Word8
consumeByte = do
  bs <- get
  case bs of
    []       -> throwError TruncatedInput
    (b:rest) -> put rest >> return b

-- consumeBytes
consumeBytes :: Int -> DecodeM [Word8]
consumeBytes 0 = return []
consumeBytes n = do
  b <- consumeByte
  rest <- consumeBytes (n - 1)
  return (b : rest)

-- zigzagEncode32
-- Maps negative numbers to positive ones so varints stay
-- small for values near zero.
zigzagEncode32 :: Int32 -> Word64
zigzagEncode32 n =
  let n' = fromIntegral n :: Int64
  in fromIntegral (shiftL n' 1 `xor` shiftR n' 31)

-- zigzagDecode32
zigzagDecode32 :: Word64 -> Int32
zigzagDecode32 n =
  fromIntegral (shiftR n 1) `xor` negate (fromIntegral (n .&. 1))

-- zigzagEncode64
zigzagEncode64 :: Int64 -> Word64
zigzagEncode64 n =
  fromIntegral (shiftL n 1 `xor` shiftR n 63)

-- zigzagDecode64
zigzagDecode64 :: Word64 -> Int64
zigzagDecode64 n =
  fromIntegral (shiftR n 1) `xor` negate (fromIntegral (n .&. 1))

-- encodeDouble
-- Little-endian 8 bytes via IEEE 754.
encodeDouble :: Double -> [Word8]
encodeDouble d =
  let w = castDoubleToWord64 d
  in [fromIntegral (shiftR w (i * 8) .&. 0xff) | i <- [0..7]]

-- decodeDoubleM
decodeDoubleM :: DecodeM Double
decodeDoubleM = do
  bytes <- consumeBytes 8
  let pairs = zip [0 :: Int ..] bytes
      w = foldl (\acc (i, b) -> acc .|. (fromIntegral b `shiftL` (i * 8)))
                (0 :: Word64) pairs
  return (castWord64ToDouble w)

-- wireTypeFor
-- Determine the wire type for a given field type.
wireTypeFor :: FieldType -> Int
wireTypeFor (PrimType TInt32)  = wireTypeVarint
wireTypeFor (PrimType TInt64)  = wireTypeVarint
wireTypeFor (PrimType TUInt32) = wireTypeVarint
wireTypeFor (PrimType TUInt64) = wireTypeVarint
wireTypeFor (PrimType TBool)   = wireTypeVarint
wireTypeFor (PrimType TDouble) = wireType64Bit
wireTypeFor (PrimType TString) = wireTypeLenDel
wireTypeFor (PrimType TBytes)  = wireTypeLenDel
wireTypeFor (RefType _)        = wireTypeLenDel

-- emit
-- Append bytes to the encoding state.
emit :: [Word8] -> EncodeM ()
emit bs = modify (++ bs)

-- encodeFieldKey
encodeFieldKey :: Int -> Int -> [Word8]
encodeFieldKey tag wtype =
  encodeVarint (fromIntegral (shiftL tag 3 .|. wtype))

-- encodeMsg
-- Encode a full message value.
encodeMsg :: String -> Value -> EncodeM ()
encodeMsg name (VMessage fields) = do
  ctx <- ask
  case M.lookup name ctx of
    Nothing     -> throwError (UnknownMessage name)
    Just msgDef -> do
      let fieldMap = M.fromList [(fieldName fd, fd) | fd <- msgFields msgDef]
      mapM_ (encodeNamedField name fieldMap) fields
encodeMsg _ _ = throwError (TypeMismatch "expected VMessage")

-- encodeNamedField
encodeNamedField :: String -> M.Map String FieldDef -> (String, Value) -> EncodeM ()
encodeNamedField msgN fieldMap (fname, fval) =
  case M.lookup fname fieldMap of
    Nothing -> throwError (UnknownFieldName msgN fname)
    Just fd -> encodeFieldValue fd fval

-- encodeFieldValue
-- Encode a field value, respecting its modifier.
encodeFieldValue :: FieldDef -> Value -> EncodeM ()
encodeFieldValue fd VNull =
  case fieldModifier fd of
    Optional -> return ()
    _        -> throwError (TypeMismatch "VNull for non-optional field")
encodeFieldValue fd (VRepeated vs) =
  case fieldModifier fd of
    Repeated -> mapM_ (encodeSingleField fd) vs
    _        -> throwError (TypeMismatch "VRepeated for non-repeated field")
encodeFieldValue fd val =
  encodeSingleField fd val

-- encodeSingleField
-- Encode one value with its tag and wire type.
encodeSingleField :: FieldDef -> Value -> EncodeM ()
encodeSingleField fd val = do
  let tag = fieldTag fd
      wtype = wireTypeFor (fieldType fd)
  emit (encodeFieldKey tag wtype)
  case fieldType fd of
    PrimType prim -> encodePrimValue prim val
    RefType name  -> encodeNestedMessage name val

-- encodePrimValue
encodePrimValue :: PrimTy -> Value -> EncodeM ()
encodePrimValue TInt32  (VInt32 n)  = emit (encodeVarint (zigzagEncode32 n))
encodePrimValue TInt64  (VInt64 n)  = emit (encodeVarint (zigzagEncode64 n))
encodePrimValue TUInt32 (VUInt32 n) = emit (encodeVarint (fromIntegral n))
encodePrimValue TUInt64 (VUInt64 n) = emit (encodeVarint n)
encodePrimValue TBool   (VBool b)   = emit (encodeVarint (if b then 1 else 0))
encodePrimValue TDouble (VDouble d) = emit (encodeDouble d)
encodePrimValue TString (VString s) =
  let bytes = map (fromIntegral . fromEnum) s
  in emit (encodeVarint (fromIntegral (length bytes))) >> emit bytes
encodePrimValue TBytes  (VBytes bs) =
  emit (encodeVarint (fromIntegral (length bs))) >> emit bs
encodePrimValue _ _ =
  throwError (TypeMismatch "value does not match field type")

-- encodeNestedMessage
-- Encode the inner message first, then wrap as length-delimited.
encodeNestedMessage :: String -> Value -> EncodeM ()
encodeNestedMessage name val = do
  ctx <- ask
  case runExcept (runReaderT (execStateT (encodeMsg name val) []) ctx) of
    Left err    -> throwError err
    Right bytes -> do
      emit (encodeVarint (fromIntegral (length bytes)))
      emit bytes

-- decodeMsg
-- Decode a full message from the byte stream.
decodeMsg :: String -> DecodeM Value
decodeMsg name = do
  ctx <- ask
  case M.lookup name ctx of
    Nothing     -> throwError (UnknownMessage name)
    Just msgDef -> do
      let tagMap = M.fromList [(fieldTag fd, fd) | fd <- msgFields msgDef]
      decoded <- decodeFields tagMap M.empty
      let result = map (buildFieldResult decoded) (msgFields msgDef)
      return (VMessage result)

-- buildFieldResult
buildFieldResult :: M.Map String [Value] -> FieldDef -> (String, Value)
buildFieldResult decoded fd =
  let fname = fieldName fd
  in case (fieldModifier fd, M.lookup fname decoded) of
       (Required, Just [v])  -> (fname, v)
       (Required, Just vs)   -> (fname, head vs)
       (Required, Nothing)   -> (fname, VNull)
       (Optional, Just [v])  -> (fname, v)
       (Optional, _)         -> (fname, VNull)
       (Repeated, Just vs)   -> (fname, VRepeated vs)
       (Repeated, Nothing)   -> (fname, VRepeated [])

-- decodeFields
-- Read fields from the byte stream until it is empty.
decodeFields :: M.Map Int FieldDef -> M.Map String [Value]
             -> DecodeM (M.Map String [Value])
decodeFields tagMap acc = do
  remaining <- get
  if null remaining
    then return acc
    else do
      key <- decodeVarintM
      let tag = fromIntegral (shiftR key 3)
          wtype = fromIntegral (key .&. 7)
      case M.lookup tag tagMap of
        Nothing -> do
          skipField wtype
          decodeFields tagMap acc
        Just fd -> do
          val <- decodeFieldValue fd wtype
          let fname = fieldName fd
              acc' = M.insertWith (\new old -> old ++ new) fname [val] acc
          decodeFields tagMap acc'

-- decodeFieldValue
-- Decode a single field value based on its type and wire type.
decodeFieldValue :: FieldDef -> Int -> DecodeM Value
decodeFieldValue fd wtype =
  case fieldType fd of
    PrimType prim -> decodePrimValue prim wtype
    RefType refName ->
      if wtype /= wireTypeLenDel
        then throwError (TypeMismatch "expected length-delimited for message")
        else do
          len <- fromIntegral <$> decodeVarintM
          msgBytes <- consumeBytes len
          ctx <- ask
          case runExcept (runReaderT (evalStateT (decodeMsg refName) msgBytes) ctx) of
            Left err  -> throwError err
            Right val -> return val

-- decodePrimValue
decodePrimValue :: PrimTy -> Int -> DecodeM Value
decodePrimValue TInt32  0 = VInt32  . zigzagDecode32 <$> decodeVarintM
decodePrimValue TInt64  0 = VInt64  . zigzagDecode64 <$> decodeVarintM
decodePrimValue TUInt32 0 = VUInt32 . fromIntegral   <$> decodeVarintM
decodePrimValue TUInt64 0 = VUInt64                   <$> decodeVarintM
decodePrimValue TBool   0 = do
  n <- decodeVarintM
  return (VBool (n /= 0))
decodePrimValue TDouble 1 = VDouble <$> decodeDoubleM
decodePrimValue TString 2 = do
  len <- fromIntegral <$> decodeVarintM
  bytes <- consumeBytes len
  return (VString (map (toEnum . fromIntegral) bytes))
decodePrimValue TBytes 2 = do
  len <- fromIntegral <$> decodeVarintM
  bytes <- consumeBytes len
  return (VBytes bytes)
decodePrimValue _ wt =
  throwError (TypeMismatch ("unexpected wire type " ++ show wt))

-- skipField
-- Skip an unknown field based on its wire type.
skipField :: Int -> DecodeM ()
skipField 0 = do _ <- decodeVarintM; return ()
skipField 1 = do _ <- consumeBytes 8; return ()
skipField 2 = do
  len <- fromIntegral <$> decodeVarintM
  _ <- consumeBytes len
  return ()
skipField 5 = do _ <- consumeBytes 4; return ()
skipField wt = throwError (InvalidWireType wt)
