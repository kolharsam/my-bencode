{-# LANGUAGE FlexibleInstances #-}

module ReadBType (
  BType(..)
  , readInput
) where


import qualified BInteger            as BI
import qualified BString             as BT
import qualified BList               as BL
import qualified BMap                as BM

import qualified Data.ByteString     as BS

import           Control.Applicative (Alternative (empty, (<|>)))
import           Text.Read           (readEither)

data BType = BInt BI.BInteger
           | BStr BT.BString
           | BLis BL.BList
           | Bmap BM.BMap
  deriving(Show)

instance Alternative (Either String) where
  empty = Left "There's nothing here to compare!"
  Left _  <|> Right v = Right v
  Right v <|> _       = Right v
  Left _  <|> Left _  = Left "It is neither an integer nor a string nor a list nor a map!"

-- instance Show (Either String BI.BInteger) where
--   show x = case x of
--     Left s -> show s
--     Right v -> show v

readInt :: String -> Either String BI.BInteger
readInt = fmap (BI.mkBInteger . show . toInteger) . readEither

readBList :: String -> Either String BL.BList
readBList x
  |BL.isFirstOpening x && BL.isLastClosing x = Right $ BL.mkBList $ BL.stringTogether $ BL.mkStrList x
  |otherwise                                 = Left "This is not a valid list"

readBMap :: String -> Either String BM.BMap
readBMap x
  |BM.isFirstOpening x && BM.isLastClosing x = undefined  -- Right $ BM.mkBMap $ BM.stringTogether $ BM.mkStrMap x
  |otherwise                                 = Left "This is not a valid map"

readInput :: String -> Either String BType
-- readInput x = BInt <$> (readInt x) <|> BLis <$> (readBList x) <|> BStr <$> BST.mkBString <$> readEither x
readInput x = case readInt x of
  Right v -> Right $ BInt v
  Left _ -> case readBList x of
    Right v -> Right $ BLis v
    Left _ -> case readBMap x of
      Right v -> Right $ Bmap v
      Left _  -> Right $ BStr $ BT.mkBString x -- this is the last resort, taken as a string
