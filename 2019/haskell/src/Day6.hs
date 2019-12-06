--{-# LANGUAGE OverloadedStrings #-}
module Day6 where

import           Util

import           Text.Megaparsec            (sepBy1)
import           Text.Megaparsec.Char       (char)
import           Text.Megaparsec.Char.Lexer (decimal, signed)

