module BString(
  BString(..)
  , mkBString
  , fromBString
  , encodeStr
) where

import qualified Data.ByteString           as BS 
import qualified Data.Text                 as T
import           Data.Text.Encoding        (encodeUtf8, decodeUtf8)
import           Text.Read                 (readEither)

newtype BString = BString { getBString :: BS.ByteString }
  deriving(Read)

instance Show BString where
  show x = show $ getBString x

encodeLength :: String -> String
encodeLength s = show (length s) ++ ":" ++ s   

mkBString :: String -> BString
mkBString x = BString $ encodeUtf8 . T.pack . encodeLength $ x

removeEncodedStr :: String -> String
removeEncodedStr = drop 2

fromBString :: BString -> String
fromBString = removeEncodedStr . T.unpack . decodeUtf8 . getBString 

encodeStr :: String -> String
encodeStr = encodeLength
