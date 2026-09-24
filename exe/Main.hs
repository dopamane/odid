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
    PackODID fs fM -> do
      msgs <- mapM decodeFile fs
      let msg = Msg (MsgHdr 2 Pack) $ PackBdy 0x19 (fromIntegral $ length msgs) msgs
      maybe BS.putStr BS.writeFile fM $ runPut $ put msg

prefs' :: ParserPrefs
prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

pinfo :: String -> ParserInfo CLI
pinfo v = info (parser <**> simpleVersioner v <**> helper) $ progDesc "Open Drone ID"

data CLI = ReadODID (Maybe String) | WriteODID Msg (Maybe String)
  | PackODID [String] (Maybe String)

parser :: Parser CLI
parser = hsubparser $ mconcat
  [ command "r" $ info (ReadODID <$> optional fileArg) $ progDesc "Read ODID data"
  , command "w" $ info (WriteODID <$> msgParser <*> optional fileArg) $ progDesc "Write ODID data"
  , command "p" $ info (PackODID <$> some fileOpt <*> optional fileArg) $ progDesc "Pack ODID data"
  ]

msgParser :: Parser Msg
msgParser = hsubparser $ mconcat
  [ command "basic" $ info basicIDParser $ progDesc "Basic ID"
  , command "loc" $ info locationParser $ progDesc "Location/Vector"
  , command "self" $ info selfIDParser $ progDesc "Self ID"
  , command "sys" $ info systemParser $ progDesc "System"
  , command "op" $ info opIDParser $ progDesc "Operator ID"
  ]

basicIDParser :: Parser Msg
basicIDParser = fmap (Msg $ MsgHdr 2 BasicIDTy) $
  BasicIDBdy <$> parseIDType <*> parseUAType <*> parseUASID <*> parseRsvdBytes

parseRsvdBytes :: Parser ByteString
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
selfIDParser = fmap (Msg $ MsgHdr 2 SelfIDTy) $
  SelfIDBdy <$> option auto (short 't' <> value 0 <> showDefault <> help "Description type")
    <*> pad 23 `fmap` strArgument (metavar "DESC" <> help "Description")

opIDParser :: Parser Msg
opIDParser = fmap (Msg $ MsgHdr 2 OperatorID) $
  OpIDBdy <$> parseOpIDTy <*> parseOpID <*> parseRsvdBytes
  where
    parseOpIDTy = option auto $ short 't' <> help "Operator ID type"
    parseOpID = fmap (pad 20) $ strArgument $ metavar "ID" <> help "ASCII text"

pad :: Int64 -> String -> ByteString
pad n s = BS.take n $ BSC.pack s <> BS.replicate n 0x00

fileArg :: Parser String
fileArg = strArgument $ metavar "FILE" <> completer (bashCompleter "file")
  <> help "Optional binary input file otherwise stream STDIN."

fileOpt :: Parser String
fileOpt = strOption $ short 'm' <> long "msg" <> metavar "FILE"
  <> help "Message file(s) to pack"

locationParser :: Parser Msg
locationParser = fmap (Msg (MsgHdr 2 Location) . LocBdy) $
  LocMsg <$> opStatusParser <*> switch (long "flag-rsvd" <> help "Reserved flag")
    <*> heightTypeParser
    <*> switch (long "180" <> help "E/W direction segment switch. >=180 active otherwise <180")
    <*> speedMultParser <*> option auto (long "track-dir" <> help "Track direction 0-359 deg.")
    <*> option auto (long "speed" <> help "Ground speed m/s")
    <*> option auto (long "vert-speed" <> help "Vertical speed m/s")
    <*> parseLat <*> parseLon
    <*> option auto (long "pres-alt" <> help "Pressure altitude")
    <*> option auto (long "geo-alt" <> help "Geodetic altitude")
    <*> option auto (long "height") <*> vertAccParser "vert-acc"
    <*> horizAccParser <*> vertAccParser "baro-acc" <*> speedAccParser
    <*> option auto (long "timestamp") <*> option auto (long "tstamp-acc-rsvd")
    <*> option auto (long "tstamp-acc") <*> option auto (long "loc-rsvd")

parseLat :: Parser Double
parseLat = option auto $ long "lat" <> help "Latitude"

parseLon :: Parser Double
parseLon = option auto $ long "lon" <> help "Longitude"

opStatusParser :: Parser OpStatus
opStatusParser = asum
  [ flag Undeclared Undeclared $ long "undeclared" <> help "Default operational status"
  , flag' Ground $ long "ground"
  , flag' Airborne $ long "airborne"
  , flag' Emergency $ long "emergency"
  , flag' RemoteIDSystemFailure $ long "failure" <> help "Remote ID system failure"
  , option auto $ long "op-rsvd" <> help "Reserved"
  ]

heightTypeParser :: Parser HeightType
heightTypeParser = flag AboveTakeoff AboveTakeoff
  (long "above-takeoff" <> help "Default height type") <|>
  flag' AGL (long "agl" <> help "Above ground level height type")

speedMultParser :: Parser Bool
speedMultParser = flag False False (long "25" <> help "Default speed multiplier x0.25")
  <|> flag' True (long "75" <> help "Speed multiplier x0.75")

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

systemParser :: Parser Msg
systemParser = fmap (Msg (MsgHdr 2 System) . SysBdy) $
  SysMsg <$> parseClassType <*> parseOpLocSrc <*> parseLat
    <*> parseLon <*> parseAreaCnt <*> parseAreaRad <*> parseAreaCeil
    <*> parseAreaFlor <*> parseClassCat <*> parseClassClass
    <*> parseOpAlt <*> parseSysTimestamp <*> parseSysRsvd

parseClassType :: Parser ClassType
parseClassType = asum
  [ flag ClassTypeUndeclared ClassTypeUndeclared $ long "undeclared"
      <> help "Undeclared class type, default"
  , flag' EuroUnion $ long "eu" <> help "European Union class type"
  , option auto $ long "ct-rsvd" <> help "Class type reserved 2-7"
  ]

parseOpLocSrc :: Parser OpLocSrc
parseOpLocSrc = asum
  [ flag Takeoff Takeoff $ long "takeoff" <> help "Default operator location"
  , flag' Dynamic $ long "dynamic" <> help "Dynamic operator location"
  , flag' Fixed $ long "fixed" <> help "Fixed operator location"
  ]

parseAreaCnt :: Parser Word16
parseAreaCnt = option auto $ long "area-cnt" <> value 1 <> showDefault
  <> help "Number of aircraft in area, group or formation"

parseAreaRad :: Parser Integer
parseAreaRad = option auto $ long "area-rad" <> value 0 <> showDefault
  <> help "Radius in meters of cylindrical area of group or formation"

parseAreaCeil :: Parser Double
parseAreaCeil = option auto $ long "area-ceil" <> value 0 <> showDefault
  <> help "Group operations ceiling in meters"

parseAreaFlor :: Parser Double
parseAreaFlor = option auto $ long "area-floor" <> value 0 <> showDefault
  <> help "Group operations floor in meters"

parseClassCat :: Parser ClassCat
parseClassCat = asum
  [ flag Undefined Undefined $ long "undefined" <> help "Class category undefined. Default."
  , flag' Open $ long "open" <> help "Class category open"
  , flag' Specific $ long "specific" <> help "Class category specific"
  , flag' Certified $ long "certified" <> help "Class category certified"
  , option auto $ long "cat-rsvd" <> help "Class category reserved"
  ]

parseClassClass :: Parser Word8
parseClassClass = option auto $ long "class" <> value 0 <> showDefault
  <> help "UA Classification class low nibble 0-15"

parseOpAlt :: Parser Double
parseOpAlt = option auto $ long "op-alt" <> value 0 <> showDefault
  <> help "Operator altitude meters"

parseSysTimestamp :: Parser Word32
parseSysTimestamp = option auto $ long "timestamp" <> value 0 <> showDefault
  <> help "32 bit timestamp in seconds since 00:00:00 01/01/2019"

parseSysRsvd :: Parser Word8
parseSysRsvd = option auto $ long "sys-rsvd" <> value 0 <> showDefault
  <> help "Reserved"
