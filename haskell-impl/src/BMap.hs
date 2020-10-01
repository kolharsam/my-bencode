module BMap (
  BMap(..)
  , mkBMap
  , isLastClosing
  , isFirstOpening
) where

import qualified BInteger           as BI
import qualified BString            as BT
import qualified BList              as BL

import qualified Data.ByteString    as BS
import qualified Data.Text          as T

import           Data.List          (concat, intercalate)
import           Data.Maybe         (fromMaybe)
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)

newtype BMap = BMap { getBMap :: BS.ByteString }
  deriving(Read)

instance Show BMap where
  -- need more details on here
  show x = show $ getBMap x

encodeMap :: String -> String
encodeMap x = "d" ++ x ++ "e"

mkBMap :: String -> BMap
mkBMap x = if x == "{}" then BMap $ encodeUtf8 . T.pack $ "de" else BMap $ encodeUtf8 . T.pack . encodeMap $ x

openingBracket :: Char
openingBracket = '{'

closingBracket :: Char
closingBracket = '}'

isFirstOpening :: String -> Bool
isFirstOpening x = head x == openingBracket

isLastClosing :: String -> Bool
isLastClosing x = last x == closingBracket
