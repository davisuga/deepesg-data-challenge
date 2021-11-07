{-# LANGUAGE OverloadedStrings #-}

module Main where

import Codec.Xlsx (Cell (_cellValue), Table (tblName), Worksheet (_wsCells, _wsTables), cellValue, fromRows, ixCell, ixSheet, toRows, toXlsx)
import Codec.Xlsx.Parser (toXlsx)
import Codec.Xlsx.Types (CellValue (CellDouble, CellText), Xlsx (_xlSheets))
import Codec.Xlsx.Types.Cell (Cell (_cellValue), CellMap (..))
import qualified Data.ByteString.Lazy as BS
import Data.Map.Lazy (fromList, mapWithKey)
import Data.Maybe (isJust)

-- Discard the first line
handleCell (1, _) _ = Nothing
handleCell (line, row) Nothing = Nothing
handleCell (line, row) (Just (CellText cellVal)) = Just (show line ++ ":" ++ show row ++ show cellVal)
handleCell (line, row) (Just (CellDouble cellVal)) = Just (show line ++ ":" ++ show row ++ show cellVal)
handleCell (line, row) (Just cellVal) = Just (show line ++ ":" ++ show row ++ show cellVal)

getWorksheetCellsFromFile path = _wsCells . snd . head . _xlSheets . toXlsx <$> BS.readFile path

main :: IO ()
main = do
  cellMap <- getWorksheetCellsFromFile "../input/general_ledger.xlsx"
  let cellValues = _cellValue <$> cellMap
  let result = mapWithKey handleCell cellValues

  print result
