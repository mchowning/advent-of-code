{-# LANGUAGE OverloadedStrings #-}
module Util (Parser, parse', parseInput, processEither) where

import Data.Void (Void)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Megaparsec (Parsec, errorBundlePretty, parse)
import Text.Megaparsec.Error (ParseErrorBundle)

type Parser = Parsec Void Text

parseInput :: Parser a -> Text -> IO a
parseInput parser filename = do
  input <- T.readFile (T.unpack ("../inputs/" <> filename))
  return $ parse' parser filename input

parse' :: Parser a -> Text -> Text -> a
parse' parser filename input = processEither (parse parser (T.unpack filename) input)

processEither :: Either (ParseErrorBundle Text Void) a -> a
processEither (Left  e ) = error (errorBundlePretty e)
processEither (Right rs) = rs
