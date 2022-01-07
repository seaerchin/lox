module Lib where

import Options.Applicative (Alternative ((<|>)), Parser, execParser, helper, (<**>))
import Options.Applicative.Builder
import Relude

data Entry = File Text | Prompt deriving (Show, Eq)

-- utility functions for easy lookup
todo :: Text -> a
todo msg = error ("TODO: " <> msg)

-- exec is the main entrypoint into lox
exec :: IO ()
exec =
  execParser (info (combinedParser <**> helper) loxInfo)
    >>= ( \case
            File s -> todo $ "Add file implementation;" <> " " <> "Queried for: " <> s
            Prompt -> todo "Add prompt implementation"
        )
  where
    combinedParser = sourceFileParser <|> replParser -- we have to try the source file first as repl always succeeds
    loxInfo = header "Welcome to the lox interpreter." <> fullDesc <> progDesc "Either pass a path to the source file or invoke as is to enter the REPL"

sourceFileParser :: Parser Entry
sourceFileParser = argument (File <$> str) (metavar "FILENAME")

-- nothing to parse
replParser :: Parser Entry
replParser = pure Prompt