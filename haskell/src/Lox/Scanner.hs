module Lox.Scanner where

import Lox.Token
import Relude
import System.IO (hPutStrLn)
import Util

data Position = Pos
  { line :: Int,
    col :: Int
  }
  deriving (Eq, Show)

type ScannerState a = StateT Position IO a

getScannerLine :: ScannerState Int
getScannerLine = line <$> get

getScannerCol :: ScannerState Int
getScannerCol = col <$> get

scanTokens :: Show s => s -> [Token]
scanTokens = todo "convert the source string into a list of tokens"

error :: Text -> ScannerState ()
error message = do
  line <- getScannerLine
  liftIO $ report line "" message

report :: Int -> Text -> Text -> IO ()
report line whrErr message = do
  let errMsg = "[line " <> show line <> "]" <> whrErr <> ": " <> message
  hPutStrLn stderr (toString errMsg)