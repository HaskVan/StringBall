{-# LANGUAGE FlexibleInstances #-}
module Data.StringBall
       ( IsByteString(..)
       , IsLazyByteString(..)
       , IsText (..)
       , IsLazyText(..)
       , IsString'(..)
       , fromUTF8
       , fromUTF16LE
       , fromUTF16BE
       , fromUTF32LE
       , fromUTF32BE
       , utf8
       , utf16LE
       , utf16BE
       , utf32LE
       , utf32BE
       , utf8With
       , utf16LEWith
       , utf16BEWith
       , utf32LEWith
       , utf32BEWith
       , TE.lenientDecode
       , TE.strictDecode
       , TE.ignore
       , TE.replace
       )
       where

import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.String                (IsString, fromString)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.Encoding.Error   as TE
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Lazy.Encoding    as LTE

--------------------

data UTF8    t = UTF8    { fromUTF8    :: t, onErrorUTF8 :: TE.OnDecodeError }
data UTF16LE t = UTF16LE { fromUTF16LE :: t, onErrorUTF16LE :: TE.OnDecodeError }
data UTF16BE t = UTF16BE { fromUTF16BE :: t, onErrorUTF16BE :: TE.OnDecodeError }
data UTF32LE t = UTF32LE { fromUTF32LE :: t, onErrorUTF32LE :: TE.OnDecodeError }
data UTF32BE t = UTF32BE { fromUTF32BE :: t, onErrorUTF32BE :: TE.OnDecodeError }

----------

utf8 :: a -> UTF8 a
utf8 a = UTF8 a TE.strictDecode

utf16LE :: a -> UTF16LE a
utf16LE a = UTF16LE a TE.strictDecode

utf16BE :: a -> UTF16BE a
utf16BE a = UTF16BE a TE.strictDecode

utf32LE :: a -> UTF32LE a
utf32LE a = UTF32LE a TE.strictDecode

utf32BE :: a -> UTF32BE a
utf32BE a = UTF32BE a TE.strictDecode

----------

utf8With :: TE.OnDecodeError -> a -> UTF8 a
utf8With ode a = UTF8 a ode

utf16LEWith :: TE.OnDecodeError -> a -> UTF16LE a
utf16LEWith ode a = UTF16LE a ode

utf16BEWith :: TE.OnDecodeError -> a -> UTF16BE a
utf16BEWith ode a = UTF16BE a ode

utf32LEWith :: TE.OnDecodeError -> a -> UTF32LE a
utf32LEWith ode a = UTF32LE a ode

utf32BEWith :: TE.OnDecodeError -> a -> UTF32BE a
utf32BEWith ode a = UTF32BE a ode

--------------------

class IsByteString a where
  fromByteString :: B.ByteString -> a
  toByteString   :: a -> B.ByteString

instance IsByteString B.ByteString where
  fromByteString = id
  toByteString   = id

instance IsByteString LB.ByteString where
  fromByteString = LB.fromChunks . return
  toByteString = B.concat . LB.toChunks

instance IsByteString [Char] where
  fromByteString = B.unpack
  toByteString   = B.pack

instance IsByteString (UTF8 T.Text) where
  fromByteString = utf8 . TE.decodeUtf8
  toByteString   = TE.encodeUtf8 . fromUTF8

instance IsByteString (UTF16LE T.Text) where
  fromByteString = utf16LE . TE.decodeUtf16LE
  toByteString   = TE.encodeUtf16LE . fromUTF16LE

instance IsByteString (UTF16BE T.Text) where
  fromByteString = utf16BE . TE.decodeUtf16BE
  toByteString   = TE.encodeUtf16BE . fromUTF16BE

instance IsByteString (UTF32LE T.Text) where
  fromByteString = utf32LE . TE.decodeUtf32LE
  toByteString   = TE.encodeUtf32LE . fromUTF32LE

instance IsByteString (UTF32BE T.Text) where
  fromByteString = utf32BE . TE.decodeUtf32BE
  toByteString   = TE.encodeUtf32BE . fromUTF32BE

instance IsByteString LT.Text where
  fromByteString = LT.fromStrict . TE.decodeUtf8
  toByteString   = toByteString  . LTE.encodeUtf8

instance IsByteString (UTF8 LT.Text) where
  fromByteString = utf8 . LT.fromStrict . TE.decodeUtf8
  toByteString   = toByteString . LTE.encodeUtf8 . fromUTF8

instance IsByteString (UTF16LE LT.Text) where
  fromByteString = utf16LE . LT.fromStrict . TE.decodeUtf16LE
  toByteString   = toByteString . LTE.encodeUtf16LE . fromUTF16LE

instance IsByteString (UTF16BE LT.Text) where
  fromByteString = utf16BE . LT.fromStrict . TE.decodeUtf16BE
  toByteString   = toByteString . LTE.encodeUtf16BE . fromUTF16BE

instance IsByteString (UTF32LE LT.Text) where
  fromByteString = utf32LE . LT.fromStrict . TE.decodeUtf32LE
  toByteString   = toByteString . LTE.encodeUtf32LE . fromUTF32LE

instance IsByteString (UTF32BE LT.Text) where
  fromByteString = utf32BE . LT.fromStrict . TE.decodeUtf32BE
  toByteString   = toByteString . LTE.encodeUtf32BE . fromUTF32BE

--------------------

class IsLazyByteString a where
  fromLByteString :: LB.ByteString -> a
  toLByteString   :: a -> LB.ByteString

instance IsLazyByteString LB.ByteString where
  fromLByteString = id
  toLByteString   = id

instance IsLazyByteString B.ByteString where
  fromLByteString = toByteString
  toLByteString   = fromByteString

instance IsLazyByteString [Char] where
  fromLByteString = toString
  toLByteString   = fromString'

instance IsLazyByteString T.Text where
  fromLByteString = TE.decodeUtf8 . fromLByteString
  toLByteString   = toLByteString . TE.encodeUtf8

instance IsLazyByteString (UTF8 T.Text) where
  fromLByteString = utf8 . LT.toStrict . LTE.decodeUtf8
  toLByteString   = toLByteString . TE.encodeUtf8 . fromUTF8

instance IsLazyByteString (UTF16LE T.Text) where
  fromLByteString = utf16LE . LT.toStrict .  LTE.decodeUtf16LE
  toLByteString   = toLByteString . TE.encodeUtf16LE . fromUTF16LE

instance IsLazyByteString (UTF16BE T.Text) where
  fromLByteString = utf16BE . LT.toStrict . LTE.decodeUtf16BE
  toLByteString   = toLByteString . TE.encodeUtf16BE . fromUTF16BE

instance IsLazyByteString (UTF32LE T.Text) where
  fromLByteString = utf32LE . LT.toStrict . LTE.decodeUtf32LE
  toLByteString   = toLByteString . TE.encodeUtf32LE . fromUTF32LE

instance IsLazyByteString (UTF32BE T.Text) where
  fromLByteString = utf32BE . LT.toStrict . LTE.decodeUtf32BE
  toLByteString   = toLByteString . TE.encodeUtf32BE . fromUTF32BE

instance IsLazyByteString LT.Text where
  fromLByteString = LTE.decodeUtf8
  toLByteString   = LTE.encodeUtf8

instance IsLazyByteString (UTF8 LT.Text) where
  fromLByteString = utf8 . LTE.decodeUtf8
  toLByteString   = LTE.encodeUtf8 . fromUTF8

instance IsLazyByteString (UTF16LE LT.Text) where
  fromLByteString = utf16LE . LTE.decodeUtf16LE
  toLByteString   = LTE.encodeUtf16LE . fromUTF16LE

instance IsLazyByteString (UTF16BE LT.Text) where
  fromLByteString = utf16BE . LTE.decodeUtf16BE
  toLByteString   = LTE.encodeUtf16BE . fromUTF16BE

instance IsLazyByteString (UTF32LE LT.Text) where
  fromLByteString = utf32LE . LTE.decodeUtf32LE
  toLByteString   = LTE.encodeUtf32LE . fromUTF32LE

instance IsLazyByteString (UTF32BE LT.Text) where
  fromLByteString = utf32BE . LTE.decodeUtf32BE
  toLByteString   = LTE.encodeUtf32BE . fromUTF32BE

--------------------

class IsText a where
  fromText :: T.Text -> a
  toText   :: a -> T.Text

instance IsText T.Text where
  fromText = id
  toText   = id

instance IsText LT.Text where
  fromText = LT.fromStrict
  toText   = LT.toStrict

instance IsText [Char] where
  fromText = toString
  toText   = fromString

instance IsText (UTF8 B.ByteString) where
  fromText = utf8 . TE.encodeUtf8
  toText (UTF8 t ode)  = TE.decodeUtf8With ode t

instance IsText (UTF16BE B.ByteString) where
  fromText = utf16BE . TE.encodeUtf16BE
  toText (UTF16BE t ode) = TE.decodeUtf16BEWith ode t

instance IsText (UTF16LE B.ByteString) where
  fromText = utf16LE . TE.encodeUtf16LE
  toText (UTF16LE t ode)  = TE.decodeUtf16LEWith ode t

instance IsText (UTF32BE B.ByteString) where
  fromText = utf32BE . TE.encodeUtf32BE
  toText (UTF32BE t ode)  = TE.decodeUtf32BEWith ode t

instance IsText (UTF32LE B.ByteString) where
  fromText = utf32LE . TE.encodeUtf32LE
  toText (UTF32LE t ode)  = TE.decodeUtf32LEWith ode t

instance IsText (UTF8 LB.ByteString) where
  fromText = utf8 . toLByteString . TE.encodeUtf8
  toText (UTF8 t ode)  = LT.toStrict $ LTE.decodeUtf8With ode t

instance IsText (UTF16BE LB.ByteString) where
  fromText = utf16BE . toLByteString . TE.encodeUtf16BE
  toText (UTF16BE t ode)  = LT.toStrict $ LTE.decodeUtf16BEWith ode t

instance IsText (UTF16LE LB.ByteString) where
  fromText = utf16LE . toLByteString . TE.encodeUtf16LE
  toText (UTF16LE t ode)  = LT.toStrict $ LTE.decodeUtf16LEWith ode t

instance IsText (UTF32BE LB.ByteString) where
  fromText = utf32BE . toLByteString . TE.encodeUtf32BE
  toText (UTF32BE t ode)  = LT.toStrict $ LTE.decodeUtf32BEWith ode t

instance IsText (UTF32LE LB.ByteString) where
  fromText = utf32LE . toLByteString . TE.encodeUtf32LE
  toText (UTF32LE t ode)  = LT.toStrict $ LTE.decodeUtf32LEWith ode t

--------------------

class IsLazyText a where
  fromLText :: LT.Text -> a
  toLText   :: a -> LT.Text

instance IsLazyText LT.Text where
  fromLText = id
  toLText   = id

instance IsLazyText T.Text where
  fromLText = LT.toStrict
  toLText   = LT.fromStrict

instance IsLazyText String where
  fromLText = toString
  toLText   = fromString'

instance IsLazyText (UTF8 B.ByteString) where
  fromLText = utf8 . toByteString . LTE.encodeUtf8
  toLText (UTF8 t ode)  = LT.fromStrict $ TE.decodeUtf8With ode t

instance IsLazyText (UTF16BE B.ByteString) where
  fromLText = utf16BE . toByteString . LTE.encodeUtf16BE
  toLText (UTF16BE t ode)  = LT.fromStrict $ TE.decodeUtf16BEWith ode t

instance IsLazyText (UTF16LE B.ByteString) where
  fromLText = utf16LE . toByteString . LTE.encodeUtf16LE
  toLText (UTF16LE t ode)  = LT.fromStrict $ TE.decodeUtf16LEWith ode t

instance IsLazyText (UTF32BE B.ByteString) where
  fromLText = utf32BE . toByteString . LTE.encodeUtf32BE
  toLText (UTF32BE t ode)   = LT.fromStrict $ TE.decodeUtf32BEWith ode t

instance IsLazyText (UTF32LE B.ByteString) where
  fromLText = utf32LE . toByteString . LTE.encodeUtf32LE
  toLText (UTF32LE t ode)  = LT.fromStrict $ TE.decodeUtf32LEWith ode t

instance IsLazyText (UTF8 LB.ByteString) where
  fromLText = utf8 . LTE.encodeUtf8
  toLText (UTF8 t ode) = LTE.decodeUtf8With ode t

instance IsLazyText (UTF16BE LB.ByteString) where
  fromLText = utf16BE . LTE.encodeUtf16BE
  toLText (UTF16BE t ode)  = LTE.decodeUtf16BEWith ode t

instance IsLazyText (UTF16LE LB.ByteString) where
  fromLText = utf16LE . LTE.encodeUtf16LE
  toLText (UTF16LE t ode)  = LTE.decodeUtf16LEWith ode t

instance IsLazyText (UTF32BE LB.ByteString) where
  fromLText = utf32BE . LTE.encodeUtf32BE
  toLText (UTF32BE t ode) = LTE.decodeUtf32BEWith ode t

instance IsLazyText (UTF32LE LB.ByteString) where
  fromLText = utf32LE . LTE.encodeUtf32LE
  toLText (UTF32LE t ode) = LTE.decodeUtf32LEWith ode t

--------------------

class IsString a => IsString' a where
  fromString' :: String -> a
  toString    :: a -> String

instance IsString' [Char] where
  fromString' = id
  toString    = id

instance IsString' T.Text where
  fromString' = fromString
  toString    = T.unpack

instance IsString' LT.Text where
  fromString' = LT.fromStrict . T.pack
  toString    = toString . LT.toStrict

instance IsString' B.ByteString where
  fromString' = fromString
  toString    = B.unpack

instance IsString' LB.ByteString where
  fromString' = fromString
  toString    = B.unpack . toByteString

