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
import qualified BString            as BST
import qualified Data.ByteString    as BS
import           Data.List          (elemIndex, intersperse, concat)
import           Data.Maybe         (fromMaybe)
import qualified Data.Text          as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)

newtype BList = BList { getBList :: BS.ByteString }
  deriving(Read)

instance Show BList where
  -- | probably need to do some more stuff here
  show x = show $ getBList x

encodeList :: String -> String
encodeList x = "l" ++ x ++ "e"

mkBList :: String -> BList
mkBList x = case x == "[]" of
  False -> BList $ encodeUtf8 . T.pack . encodeList $ x
  True  -> BList $ encodeUtf8 . T.pack $ "le"

-- | TODO: we have to decoding stuff based on their types
decodeList :: String -> String
decodeList x = undefined

fromBInteger :: BList -> String
fromBInteger = decodeList . T.unpack . decodeUtf8 . getBList

openingBracket :: Char
openingBracket = '['

closingBracket :: Char
closingBracket = ']'

isCharBracket :: Int -> Char -> String -> Bool
isCharBracket pos char word = word !! pos == char

isFirstOpening :: String -> Bool
isFirstOpening = isCharBracket 0 openingBracket

isLastClosing :: String -> Bool
isLastClosing x = isCharBracket ((length x) - 1) closingBracket x

comma :: T.Text
comma = T.pack ","

-- FIXME: this will break if there's no ]
putTogether :: [String] -> [String]
putTogether x = putWrongSplitTogether x [] []

putWrongSplitTogether :: [String] -> [String] -> [String] -> [String]
putWrongSplitTogether [] acc _ = acc

putWrongSplitTogether (x:[]) acc tempAcc = case length tempAcc == 0 of
  True  -> reverse $ x:acc
  False -> reverse $ (stringTogetherComma $ reverse $ x:tempAcc):acc -- check case

putWrongSplitTogether (x:xs) acc tempAcc = case isFirstOpening x of
  True  -> putWrongSplitTogether xs acc (x:tempAcc)
  False -> case length tempAcc == 0 of
    True  -> putWrongSplitTogether xs (x:acc) []
    False -> case isLastClosing x of
      True  -> putWrongSplitTogether xs ((stringTogetherComma $ reverse (x:tempAcc)):acc) [] -- check case
      False -> putWrongSplitTogether xs acc (x:tempAcc)

splitStrList :: String -> [String]
splitStrList x = do
  let removeBrackets = drop 1 $ reverse $ drop 1 $ reverse x
  putTogether $ map T.unpack $ T.splitOn comma $ T.pack $ filter (/= ' ') removeBrackets -- removes whitespaces as well

-- TODO: This should handle maps too
mkStrList :: String -> [String]
mkStrList x = do
  let splitList = splitStrList x
    in
    --  trace (show splitList)
     map (\s -> case BI.encodeIntStr s of
       Right v -> v
       Left _ -> case isFirstOpening s && isLastClosing s of
         True  -> "l" ++ (stringTogether $ mkStrList s) ++ "e" -- TODO: make fn for enclosing these lists
         False -> BST.encodeStr s) splitList

stringTogether :: [String] -> String
stringTogether = foldr (++) ""

stringTogetherComma :: [String] -> String
stringTogetherComma x = concat $ intersperse "," x
