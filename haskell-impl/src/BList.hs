module BList (
  BList(..)
  , mkBList
  , isFirstOpening
  , isLastClosing
  , mkStrList
  , stringTogether
) where

-- import Debug.Trace (trace)

import qualified BInteger           as BI
import qualified BString            as BT

import qualified Data.Text          as T
import qualified Data.ByteString    as BS

import           Data.List          (concat, intercalate)
import           Data.Maybe         (fromMaybe)
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)

newtype BList = BList { getBList :: BS.ByteString }
  deriving(Read)

instance Show BList where
  -- | probably need to do some more stuff here
  show x = show $ getBList x

encodeList :: String -> String
encodeList x = "l" ++ x ++ "e"

mkBList :: String -> BList
mkBList x = if x == "[]" then BList $ encodeUtf8 . T.pack $ "le" else BList $ encodeUtf8 . T.pack . encodeList $ x

-- | TODO: we have to decoding stuff based on their types
decodeList :: String -> String
decodeList x = undefined

fromBInteger :: BList -> String
fromBInteger = decodeList . T.unpack . decodeUtf8 . getBList

openingBracket :: Char
openingBracket = '['

closingBracket :: Char
closingBracket = ']'

isFirstOpening :: String -> Bool
isFirstOpening x = head x == openingBracket

isLastClosing :: String -> Bool
isLastClosing x = last x == closingBracket

comma :: T.Text
comma = T.pack ","

-- FIXME: this will break if there's no ]
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

splitStrList :: String -> [String]
splitStrList x = do
  let removeBrackets = drop 1 $ reverse $ drop 1 $ reverse x
  putTogether $ map T.unpack $ T.splitOn comma $ T.pack $ filter (/= ' ') removeBrackets

-- TODO: This should handle maps too
mkStrList :: String -> [String]
mkStrList x = do
  let splitList = splitStrList x
    in
    --  trace (show splitList)
     map (\s -> case BI.encodeIntStr s of
       Right v -> v
       Left _  -> if s == "[]"
         then "le"
         else if isFirstOpening s && isLastClosing s
           then "l" ++ stringTogether (mkStrList s) ++ "e"
           else BT.encodeStr s
     ) splitList

stringTogether :: [String] -> String
stringTogether = concat

stringTogetherComma :: [String] -> String
stringTogetherComma = intercalate ","
