module Lib
  ( someFunc,
  )
where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- openXlsxFile :: FilePath -> IO ()
-- openXlsxFile filePath = do
--   xlsx <- readXlsx filePath
--   let sheet = head $ worksheets xlsx
--   let rows = toLists sheet
--   let header = head rows
--   let body = tail rows
-- let result = map (map (\(Cell _ _ (RichString _ str)) -> str)) body