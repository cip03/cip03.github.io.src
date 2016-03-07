-----------------------------------------------------------------------------
-- |
-- Module      : Main
-- Copyright   : (c) 2016 Călin Ardelean
-- License     : BSD-style
--
-- Maintainer  : Călin Ardelean <mmn80cpu@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- A parser for an indentation based XML syntax.
-----------------------------------------------------------------------------

module SvgParser (svgLight2Xml) where

import Text.Parsec
import Control.Monad (void)
import Data.List (intercalate)

svgLight2Xml :: String -> String
svgLight2Xml str = either show id $ parse (elementParser 0) "" str

elementParser :: Int -> Parsec String u String
elementParser lvl = do
  try $ count lvl spc
  let ind = replicate lvl ' '
  textParser lvl ind <|> do
    tag   <- many1 letter
    attrs <- manyTill attrParser eol
    kids  <- many $ elementParser $ lvl + 2
    return $ ind ++ "<" ++ tag ++
      (if null attrs then "" else " ") ++ unwords attrs ++
      (if all null kids then " />"
       else ">\n" ++ unlines kids ++ ind ++ "</" ++ tag ++ ">")

textParser :: Int -> String -> Parsec String u String
textParser lvl ind = do
  try $ string "> "
  txt  <- manyTill anyChar eol
  kids <- many $ elementParser $ lvl + 2
  return $ ind ++ txt ++ if all null kids then ""
                         else "\n" ++ intercalate "\n" kids

attrParser :: Parsec String u String
attrParser = do
  spc
  attr <- many1 $ letter <|> oneOf "-:"
  char '='
  val <- try (sep >> manyTill anyChar sep) <|> many (noneOf [' ', '\n'])
  return $ attr ++ "=" ++ case val of
    '"':_ -> val
    _     -> "\"" ++ val ++ "\""

eol :: Parsec String u ()
eol = try eof <|> void newline

spc :: Parsec String u Char
spc = char ' '

sep :: Parsec String u Char
sep = char '"'
