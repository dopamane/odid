module Main (main) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BS
import Data.ODID
import Options.Applicative
import Prettyprinter

main :: IO ()
main = do
  cli <- customExecParser prefs' pinfo
  case cli of
    ReadODID fM -> print . pretty . runGet (get :: Get Msg) =<< maybe BS.getContents BS.readFile fM
    WriteODID msg fM -> maybe BS.putStr BS.writeFile fM $ runPut $ put msg

prefs' :: ParserPrefs
prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

pinfo :: ParserInfo CLI
pinfo = info (parser <**> helper) $ progDesc "Open Drone ID"

data CLI = ReadODID (Maybe String) | WriteODID Msg (Maybe String)

parser :: Parser CLI
parser = hsubparser $ mconcat
  [ command "r" $ info (ReadODID <$> optional fileArg) $ progDesc "Read ODID data"
  , command "w" $ info (WriteODID <$> msgParser <*> optional fileArg) $ progDesc "Write ODID data"
  ]

msgParser :: Parser Msg
msgParser = undefined

fileArg :: Parser String
fileArg = strArgument $ metavar "FILE" <> completer (bashCompleter "file")
  <> help "Optional binary input file otherwise stream STDIN."
