{-# LANGUAGE NoImplicitPrelude #-}

module Lib
  ( someFunc,
  )
where

import Data.Char (Char, toUpper)
import Data.Functor ( Functor(fmap) )
import Prelude (Char, IO, flip, getLine, print, putStrLn, reverse, (/), Maybe (Nothing))
import Data.Maybe (Maybe(Just))

(<$>) :: Functor f => f a -> (a -> b) -> f b
(<$>) = flip fmap

toUpperCase :: [Char] -> [Char]
toUpperCase [] = []
toUpperCase (x : xs) = toUpper x : xs

divideBy x 0 = Nothing 
divideBy x y = Just (x / y)

someFunc :: IO ()
someFunc = do
  getLine <$> reverse <$> toUpperCase
  
  print ""

-- openXlsxFile :: FilePath -> IO ()
-- openXlsxFile filePath = do
--   xlsx <- readXlsx filePath
--   let sheet = head $ worksheets xlsx
--   let rows = toLists sheet
--   let header = head rows
--   let body = tail rows
-- let result = map (map (\(Cell _ _ (RichString _ str)) -> str)) body