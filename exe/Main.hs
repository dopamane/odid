module Main (main) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Char
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
msgParser = basicIDParser

basicIDParser :: Parser Msg
basicIDParser = fmap (Msg $ MsgHdr 2 BasicIDTy) $ BasicIDBdy <$> parseIDTy
  <*> parseUAType <*> parseUASID <*> parseRsvdBytes
  where
    parseIDTy = pure IDTypeNone
    parseRsvdBytes = pure $ BS.replicate 3 0x00

parseUAType :: Parser UAType
parseUAType = asum $ map mkFlag [None ..]
  where
    mkFlag None = flag None None $ long "none"
    mkFlag t = flag' t $ long $ map toLower $ show t

parseUASID :: Parser ByteString
parseUASID = pad <$> strOption (short 'u' <> long "uasid" <> help "UASID")
  where
    pad s = BS.take 20 $ BSC.pack s <> BS.replicate 20 0x00

fileArg :: Parser String
fileArg = strArgument $ metavar "FILE" <> completer (bashCompleter "file")
  <> help "Optional binary input file otherwise stream STDIN."
