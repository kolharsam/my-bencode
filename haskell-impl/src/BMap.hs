module BMap (
  BMap(..)
  , mkBMap
  , isLastClosing
  , isFirstOpening
  , mkStrMap
  , stringTogether
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
mkBMap x = if x == "{}"
  then BMap $ encodeUtf8 . T.pack $ "de"
  else BMap $ encodeUtf8 . T.pack . encodeMap $ x

openingBracket :: Char
openingBracket = '{'

closingBracket :: Char
closingBracket = '}'

isFirstOpening :: String -> Bool
isFirstOpening x = head x == openingBracket

isLastClosing :: String -> Bool
isLastClosing x = last x == closingBracket

removeBrackets :: String -> String
removeBrackets x = drop 1 $ reverse $ drop 1 $ reverse x

clearWhiteSpaces :: String -> String
clearWhiteSpaces = filter (/= ' ')

comma :: T.Text
comma = T.pack ","

putTogether :: [String] -> [String]
putTogether x = putWrongSplitTogether x [] []

putWrongSplitTogether :: [String] -> [String] -> [String] -> [String]
putWrongSplitTogether [] acc _ = acc

putWrongSplitTogether [x] acc tempAcc = if null tempAcc
  then reverse $ x:acc
  else reverse $ stringTogetherComma (reverse $ x : tempAcc) : acc

putWrongSplitTogether (x:xs) acc tempAcc
  |isFirstOpening x && x /= "[]"  = putWrongSplitTogether xs acc (x:tempAcc)
  |null tempAcc                   = putWrongSplitTogether xs (x:acc) []
  |isLastClosing x  && x /= "[]"  = putWrongSplitTogether xs (stringTogetherComma (reverse (x:tempAcc)):acc) []
  |otherwise                      = putWrongSplitTogether xs acc (x:tempAcc)

mkStrMap :: String -> [String]
mkStrMap s = putTogether $ map T.unpack $ T.splitOn comma $ T.pack $ clearWhiteSpaces $ removeBrackets s
  
stringTogether :: [String] -> String
stringTogether = undefined

stringTogetherComma :: [String] -> String
stringTogetherComma = intercalate ","
