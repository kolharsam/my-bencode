module BInteger(
  BInteger(..)
  , mkBInteger
  , fromBInteger
  , encodeIntStr
) where

import qualified Data.ByteString as BS 
import qualified Data.Text       as T
import Data.Text.Encoding        (encodeUtf8, decodeUtf8)
import Text.Read (readEither)

newtype BInteger = BInteger { getBInteger :: BS.ByteString }
  deriving(Read)

instance Show BInteger where
  show x = show $ getBInteger x

encodeInt :: String -> String
encodeInt s = "i" ++ s ++ "e"   

mkBInteger :: String -> BInteger
mkBInteger x = BInteger $ encodeUtf8 . T.pack . encodeInt $ x

decodeInt :: String -> String
decodeInt x = drop 1 $ reverse $ drop 1 $ reverse x

fromBInteger :: BInteger -> String
fromBInteger = decodeInt . T.unpack . decodeUtf8 . getBInteger

encodeIntStr :: String -> Either String String
encodeIntStr = fmap (encodeInt . show . fromInteger) . readEither
