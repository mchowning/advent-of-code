#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

{-# OPTIONS_GHC -Wall -Werror -fno-warn-unused-do-bind -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import Prelude hiding (FilePath)
import System.Exit
import Turtle hiding (f, d, fp)

programDescription :: Description
programDescription =
  "A simple program for testing the runtime of a haskell script.\
  \ Compiles, runs the script specified (outputting the runtime stats), \
  \ and returns the file system to its original state."

instance Monoid ExitCode where
  mempty = ExitSuccess
  mappend (ExitFailure n) _ = ExitFailure n
  mappend _ (ExitFailure n) = ExitFailure n
  mappend _ _               = ExitSuccess

main :: IO ()
main = do sourceFile <- fromText <$> options programDescription parser
          verifyFileExists sourceFile
          currentDir <- pwd
          artifactDir <- with (mktempdir currentDir "compilationArtifacts") return
          -- TODO check ExitCodes here
          compileProgram sourceFile artifactDir
          timeCompiledProgramExecution sourceFile
          cleanup sourceFile artifactDir

          return ()
  where
    parser :: Parser Text
    parser = arg validFilename "filename" "The haskell source file (.hs) to be compiled"

    validFilename :: Text -> Maybe Text
    validFilename = mfilter ("hs" `T.isSuffixOf`) . pure

    verifyFileExists :: FilePath -> IO ()
    verifyFileExists fp = do fileExists <- testfile fp
                             unless fileExists $ error "Cannot find file"

compileProgram :: FilePath -> FilePath -> IO ExitCode
compileProgram (toText -> Right f) (toText -> Right d) =
  do ec <- proc "stack" ["ghc", "--", "-O2", f, "-outputdir", d] empty
     echo "Script compiled\n"
     exitWithCodes [ec] compileError
compileProgram _ _ = error compileError

compileError :: String
compileError = "Failed to initiate compilation due to unknown error"

timeCompiledProgramExecution :: FilePath -> IO ExitCode
timeCompiledProgramExecution (toText . basename -> Right t) =
  do echo "Timing script execution..."
     proc "time" [t] empty
timeCompiledProgramExecution _ = error "Failed to parse executable name"

cleanup :: FilePath -> FilePath -> IO ExitCode
cleanup (toText . basename -> Right f) (toText -> Right d) =
  do echo "\nCleaning up..."

     ec1 <- proc "rm" ["-rf", d] empty
     ec2 <- proc "rm" [f] empty

     exitWithCodes [ec1, ec2] cleanupError
cleanup _ _ = error cleanupError

cleanupError :: String
cleanupError = "Unknown error while cleaning up compilation artifacts"

-- FIXME why can't I pass in Text here?
exitWithCodes :: [ExitCode] -> String -> IO ExitCode
exitWithCodes (mconcat -> ExitSuccess) _ = return ExitSuccess
exitWithCodes _ t                        = error t
