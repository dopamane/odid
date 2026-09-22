module Main (main) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Char
import Data.Int
import Data.ODID
import Data.Version
import Options.Applicative
import Paths_odid
import Prettyprinter

main :: IO ()
main = do
  cli <- customExecParser prefs' $ pinfo $ showVersion version
  case cli of
    ReadODID fM -> print . pretty . runGet (get :: Get Msg) =<< maybe BS.getContents BS.readFile fM
    WriteODID msg fM -> maybe BS.putStr BS.writeFile fM $ runPut $ put msg

prefs' :: ParserPrefs
prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

pinfo :: String -> ParserInfo CLI
pinfo v = info (parser <**> simpleVersioner v <**> helper) $ progDesc "Open Drone ID"

data CLI = ReadODID (Maybe String) | WriteODID Msg (Maybe String)

parser :: Parser CLI
parser = hsubparser $ mconcat
  [ command "r" $ info (ReadODID <$> optional fileArg) $ progDesc "Read ODID data"
  , command "w" $ info (WriteODID <$> msgParser <*> optional fileArg) $ progDesc "Write ODID data"
  ]

msgParser :: Parser Msg
msgParser = hsubparser $ mconcat
  [ command "basic" $ info basicIDParser $ progDesc "Basic ID"
  , command "loc" $ info locationParser $ progDesc "Location/Vector"
  , command "self" $ info selfIDParser $ progDesc "Self ID"
  , command "op" $ info opIDParser $ progDesc "Operator ID"
  ]

basicIDParser :: Parser Msg
basicIDParser = fmap (Msg $ MsgHdr 2 BasicIDTy) $
  BasicIDBdy <$> parseIDType <*> parseUAType <*> parseUASID <*> parseRsvdBytes
  where
    parseRsvdBytes = fmap readRsvd $ option auto $ long "rsvd" <> value 0
      <> showDefault <> help "Reserved 3 bytes. Example 0xaabbcc."
      where
        readRsvd :: Word32 -> ByteString
        readRsvd r = BS.pack $ fromIntegral <$> [b0, b1, 0xFF .&. r]
          where
            b0 = 0xFF .&. r `shiftR` 16
            b1 = 0xFF .&. r `shiftR` 8

parseIDType :: Parser IDType
parseIDType = asum
  [ flag IDTypeNone IDTypeNone $ long "no-id" <> help "No ID type. Default ID type."
  , flag' SerialNum $ long "serial-num" <> help (show $ pretty SerialNum)
  , flag' CAARegID $ long "caa-reg-id" <> help (show $ pretty CAARegID)
  , flag' UTMUUID $ long "utm-uuid" <> help (show $ pretty UTMUUID)
  , flag' SpecificSessionID $ long "session" <> help (show $ pretty SpecificSessionID)
  ]

parseUAType :: Parser UAType
parseUAType = asum $ map mkFlag [None ..]
  where
    mkFlag None = flag None None $ long "none"
    mkFlag GroundObstacle = flag' GroundObstacle $ long "ground-obstacle"
    mkFlag t = flag' t $ long $ map toLower $ show t

parseUASID :: Parser ByteString
parseUASID = pad 20 <$> strArgument (metavar "UASID")

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

locationParser :: Parser Msg
locationParser = parserOptionGroup "Location" $ fmap (Msg (MsgHdr 2 Location) . LocBdy) $
  LocMsg <$> opStatusParser <*> switch (long "flag-rsvd" <> help "Reserved flag")
    <*> flag AboveTakeoff AGL (long "agl" <> help "Height type")
    <*> switch (long "dir" <> help "E/W direction segment switch. >=180 active otherwise <180")
    <*> switch (long "mult" <> help "Speed multiplier. x0.75 active otherwise x0.25")
    <*> option auto (long "track-dir" <> help "Track direction 0-359 deg.")
    <*> option auto (long "speed" <> help "Ground speed m/s")
    <*> option auto (long "vert-speed" <> help "Vertical speed m/s")
    <*> option auto (long "lat" <> help "Latitude")
    <*> option auto (long "lon" <> help "Longitude")
    <*> option auto (long "pres-alt" <> help "Pressure altitude")
    <*> option auto (long "geo-alt" <> help "Geodetic altitude")
    <*> option auto (long "height") <*> vertAccParser "vert-acc"
    <*> horizAccParser <*> vertAccParser "baro-acc" <*> speedAccParser
    <*> option auto (long "timestamp") <*> option auto (long "tstamp-acc-rsvd")
    <*> option auto (long "tstamp-acc") <*> option auto (long "loc-rsvd")

opStatusParser :: Parser OpStatus
opStatusParser = asum
  [ flag' Undeclared $ long "undeclared"
  , flag' Ground $ long "ground"
  , flag' Airborne $ long "airborne"
  , flag' Emergency $ long "emergency"
  , flag' RemoteIDSystemFailure $ long "failure" <> help "Remote ID system failure"
  , option auto $ long "op-rsvd" <> help "Reserved"
  ]

vertAccParser :: String -> Parser VertAcc
vertAccParser s = asum
  [ fmap readAcc $ option auto $ long s <> help "Vertical accuracy m"
  , fmap VertAccRsvd $ option auto $ long (s ++ "-rsvd") <> help "Reserved low nibble"
  ]
  where
    readAcc :: Double -> VertAcc
    readAcc m
      | m < 1 = VertAccLT1M
      | m < 3 = VertAccLT3M
      | m < 10 = VertAccLT10M
      | m < 25 = VertAccLT25M
      | m < 45 = VertAccLT45M
      | m < 150 = VertAccLT150M
      | otherwise = VertAccGTE150M

horizAccParser :: Parser HorizAcc
horizAccParser = asum
  [ fmap readAcc $ option auto $ long "horiz-acc" <> help "Horizontal accuracy m"
  , fmap HorizAccRsvd $ option auto $ long "horiz-acc-rsvd"
      <> help "Horizontal accuracy reserved value"
  ]
  where
    readAcc :: Double -> HorizAcc
    readAcc d
      | d < 1 = LT1M
      | d < 3 = LT3M
      | d < 10 = LT10M
      | d < 30 = LT30M
      | d < 92.6 = LT005NM
      | d < 185.2 = LT01NM
      | d < 555.6 = LT03NM
      | d < 926 = LT05NM
      | d < 1852 = LT1NM
      | d < 3704 = LT2NM
      | d < 7408 = LT4NM
      | d < 18520 = LT10NM
      | otherwise = GT10NM

speedAccParser :: Parser SpeedAcc
speedAccParser = asum
  [ fmap readAcc $ option auto $ long "speed-acc" <> help "Speed accuracy m/s"
  , fmap SpeedAccRsvd $ option auto $ long "speed-acc-rsvd"
      <> help "Reserved speed accuracy low nibble"
  ]
  where
    readAcc :: Double -> SpeedAcc
    readAcc s
      | s < 0.3 = LT03MS
      | s < 1 = LT1MS
      | s < 3 = LT3MS
      | s < 10 = LT10MS
      | otherwise = GTE10MS
