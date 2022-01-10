module Lox.Scanner where

import qualified Data.Text as Text
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

-- this is more of a transcription of the java code into haskell
-- which is why we don't utilize the
scanTokens :: Text -> [Token]
scanTokens source = scanTokens' [] 0 0 (-1)
  where
    scanTokens' :: [Token] -> Int -> Int -> Int -> [Token]
    scanTokens' curTokens start current line
      | current >= Text.length source =
        let eof = Token EOF "" Nothing line
         in curTokens <> [eof]
      | otherwise =
        let newStart = current
            newToken = scanToken
         in todo "something"

scanToken = todo "This needs to be implemented"

error :: Text -> ScannerState ()
error message = do
  line <- getScannerLine
  liftIO $ report line "" message

report :: Int -> Text -> Text -> IO ()
report line whrErr message = do
  let errMsg = "[line " <> show line <> "]" <> whrErr <> ": " <> message
  hPutStrLn stderr (toString errMsg)