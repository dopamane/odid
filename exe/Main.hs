module Main (main) where

import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BS
import Data.ODID
import Options.Applicative

main :: IO ()
main = do
  cli <- customExecParser prefs' pinfo
  case cli of
    ReadODID fM -> print . runGet (get :: Get Msg) =<< maybe BS.getContents BS.readFile fM
    WriteODID -> putStrLn "Writing"

prefs' :: ParserPrefs
prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

pinfo :: ParserInfo CLI
pinfo = info (parser <**> helper) $ progDesc "Open Drone ID"

data CLI = ReadODID (Maybe String) | WriteODID

parser :: Parser CLI
parser = hsubparser $ mconcat
  [ command "r" $ info (ReadODID <$> optional fileArg) $ progDesc "Read ODID data"
  , command "w" $ info (pure WriteODID) $ progDesc "Write ODID data"
  ]

fileArg :: Parser String
fileArg = strArgument $ metavar "FILE" <> completer (bashCompleter "file")
  <> help "Optional binary input file otherwise stream STDIN."
