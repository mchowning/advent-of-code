{-# LANGUAGE OverloadedStrings #-}
module Util (Parser, processEither, parseInput, unsafeParse, parse') where

import Data.Void (Void)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Megaparsec (Parsec, errorBundlePretty, parse)
import Text.Megaparsec.Error (ParseErrorBundle)

type Parser = Parsec Void Text

parseInput :: Parser a
           -> Text -- path to file source of input
           -> IO a
parseInput parser filename = do
  input <- T.readFile (T.unpack ("../inputs/" <> filename))
  return $ unsafeParse parser input

unsafeParse :: Parser a
            -> Text -- input
            -> a
unsafeParse parser = processEither . parse' parser

parse' :: Parser a
       -> Text
       -> Either (ParseErrorBundle Text Void) a
parse' parser = parse parser ""

processEither :: Either (ParseErrorBundle Text Void) a -> a
processEither (Left  e ) = error (errorBundlePretty e)
processEither (Right rs) = rs