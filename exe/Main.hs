module Main (main) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Char
import Data.Int
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
msgParser = basicIDParser <|> selfIDParser <|> opIDParser

basicIDParser :: Parser Msg
basicIDParser = parserOptionGroup "Basic ID" $ fmap (Msg $ MsgHdr 2 BasicIDTy) $
  BasicIDBdy <$> parseIDType <*> parseUAType <*> parseUASID <*> parseRsvdBytes
  where
    parseRsvdBytes = pure $ BS.replicate 3 0x00

parseIDType :: Parser IDType
parseIDType = serialNum <|> caaregid <|> utmuuid <|> specSess <|> pure IDTypeNone
  where
    serialNum = flag' SerialNum $ long "serial-num" <> help (show $ pretty SerialNum)
    caaregid = flag' CAARegID $ long "caa-reg-id" <> help (show $ pretty CAARegID)
    utmuuid = flag' UTMUUID $ long "utm-uuid" <> help (show $ pretty UTMUUID)
    specSess = flag' SpecificSessionID $ long "session" <> help (show $ pretty SpecificSessionID)

parseUAType :: Parser UAType
parseUAType = asum $ map mkFlag [None ..]
  where
    mkFlag None = flag None None $ long "none"
    mkFlag GroundObstacle = flag' GroundObstacle $ long "ground-obstacle"
    mkFlag t = flag' t $ long $ map toLower $ show t

parseUASID :: Parser ByteString
parseUASID = pad 20 <$> strOption (short 'u' <> long "uasid" <> help "UASID")

selfIDParser :: Parser Msg
selfIDParser = parserOptionGroup "Self ID" $ fmap (Msg $ MsgHdr 2 SelfIDTy) $
  SelfIDBdy <$> option auto (short 't' <> value 0 <> help "Description type")
    <*> pad 23 `fmap` strOption (short 's' <> help "Description")

opIDParser :: Parser Msg
opIDParser = parserOptionGroup "Operator ID" $ fmap (Msg $ MsgHdr 2 OperatorID) $
  OpIDBdy <$> parseOpIDTy <*> parseOpID <*> parseOpIDRsvd
  where
    parseOpIDTy = option auto $ short 't' <> help "Operator ID type"
    parseOpID = fmap (pad 20) $ strArgument $ metavar "ID" <> help "ASCII text"
    parseOpIDRsvd = pure $ BS.replicate 3 0x00

pad :: Int64 -> String -> ByteString
pad n s = BS.take n $ BSC.pack s <> BS.replicate n 0x00

fileArg :: Parser String
fileArg = strArgument $ metavar "FILE" <> completer (bashCompleter "file")
  <> help "Optional binary input file otherwise stream STDIN."
