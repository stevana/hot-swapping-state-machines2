{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}

module Codec where

import Data.Bifunctor
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Text.Read

import Message

------------------------------------------------------------------------

data DecodeError = DecodeError String
  deriving Show

displayDecodeError :: DecodeError -> String
displayDecodeError (DecodeError s) = s

data Codec a b = Codec
  { decode :: ByteString -> Either DecodeError a
  , encode :: b -> ByteString
  }

readShowCodec :: (Read a, Show b) => Codec a b
readShowCodec = Codec
  { decode = bimap DecodeError id . readEither . BS8.unpack
  , encode = BS8.pack . show
  }

idCodec :: Codec (Msg ByteString) (Msg ByteString)
idCodec = Codec
  { decode = Right . Item_
  , encode = \case
      Item _ bs -> bs
      _otherwise -> error "idCodec: non item"
  }
