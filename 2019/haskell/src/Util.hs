{-# LANGUAGE OverloadedStrings #-}
module Util (Parser, parseInput) where

import Data.Void (Void)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Megaparsec (Parsec, errorBundlePretty, runParser)

type Parser = Parsec Void Text

parseInput :: Parser a -> Text -> IO a
parseInput parser filename = do
  input <- T.readFile (T.unpack ("../inputs/" <> filename))
  return $ parse parser filename input

parse :: Parser a -> Text -> Text -> a
parse parser filename input = processEither (runParser parser (T.unpack filename) input)
  where
    processEither (Left  e ) = error (errorBundlePretty e)
    processEither (Right rs) = rs