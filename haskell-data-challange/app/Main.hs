{-# LANGUAGE OverloadedStrings #-}

module Main where

import Codec.Xlsx (Cell (_cellValue), Table (tblName), Worksheet (_wsCells, _wsTables), cellValue, fromRows, ixCell, ixSheet, toRows, toXlsx)
import Codec.Xlsx.Parser (toXlsx)
import Codec.Xlsx.Types (Cell (Cell), CellValue (CellDouble, CellText), Xlsx (_xlSheets))
import Codec.Xlsx.Types.Cell (Cell (_cellValue), CellMap (..))
import qualified Data.ByteString.Lazy as BS
import Data.Char (toUpper)
import Data.Functor ((<&>))
import Data.List.Split (splitOn)
import Data.Map (Map, empty, foldlWithKey, insert, insertWith)
import Data.Map.Lazy (empty, fromList, mapWithKey)
import Data.Maybe (isJust)
import Data.Text (unpack)
import Data.Tree

toRowList cellMap = getRowValues <$> toRows cellMap

toUpperCase :: [Char] -> [Char]
toUpperCase [] = []
toUpperCase (x : xs) = toUpper x : xs

main :: IO ()
main = do
  generalLedgerCellMap <- getCellMapFromFile "../input/general_ledger.xlsx"
  chartOfAccountsCellMap <- getCellMapFromFile "../input/chart_of_accounts.xlsx"
  let generalLedgerRows = toRowList generalLedgerCellMap
  let chartOfAccountsRows = toRowList chartOfAccountsCellMap

  let chartOfAccountsMap = foldl upsertRow Data.Map.empty chartOfAccountsRows
  let generalLedgerMap = foldl upsertRow chartOfAccountsMap generalLedgerRows

  -- mapM_ print generalLedgerMap
  print generalLedgerMap

data AccountNode
  = AccountBranch
      { accountName :: String,
        accountBalance :: Double,
        accountChildren :: [AccountNode]
      }
  | AccountLeaf
      { accountName :: String,
        accountBalance :: Double
      }
  deriving (Eq)

instance Show AccountNode where
  show (AccountBranch name children balance) = name ++ "\n" ++ show children
  show (AccountLeaf name balance) = name ++ "├──" ++ show balance ++ "\n"

splitByDot :: String -> [String]
splitByDot = splitOn "."

makeNode :: [String] -> AccountNode
makeNode label =
  AccountBranch (head label) 0 (mkNode label)
  where
    mkNode label =
      if length label == 1
        then []
        else mkNode (tail label)

-- Creates a map {account: value} from the cell values
upsertRow :: Map String Double -> [Maybe CellValue] -> Map String Double
--Ignore header
upsertRow acc (Just (CellText "account") : _) = acc
upsertRow acc (Just (CellText key) : Just (CellDouble value) : xs) = insertWith (+) (unpack key) value acc
upsertRow acc (Just (CellText key) : _) = insert (unpack key) 0 acc
upsertRow acc _ = acc

buildNode (CellText x) = Just (makeNode (splitByDot (unpack x)))
buildNode _ = Nothing

b1 = AccountBranch "1" 0 [AccountBranch "2" 0 [AccountBranch "3" 0 [AccountLeaf "4" 4.0]]]

getCellMapFromFile :: FilePath -> IO CellMap
-- You usually need to use lenses to access the cell map, but it would over complicate.
getCellMapFromFile path = _wsCells . snd . head . _xlSheets . toXlsx <$> BS.readFile path

-- Discarding cell styles and formulas, keeping the values only
getRowValues :: (Int, [(Int, Cell)]) -> [Maybe CellValue]
getRowValues (_, rowColumns) = map (_cellValue . snd) rowColumns
