{-# LANGUAGE OverloadedStrings #-}

-- | Open Drone ID
module Data.ODID
  ( ODID(..), readODID, writeODID
  , UAType(..), readUAType
  ) where

import Data.Binary.Get
import Data.Bits
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Word
import Numeric
import Prettyprinter

data ODID = ODID

readODID :: ByteString -> Either String ODID
readODID = undefined

writeODID :: ODID -> ByteString
writeODID = undefined

-- | Unmanned aircraft
data UAType
  = None | Aeroplane | Heli | Gyro | Hybrid | Ornith | Glider | Kite | FreeBalloon
  | CaptiveBalloon | Airship | Parachute | Rocket | TetheredPwrAircraft | GroundObstacle
  | Other
  deriving (Eq, Enum, Read, Show)

instance Pretty UAType where
  pretty ua = case ua of
    None -> "None"
    Aeroplane -> "Aeroplane"
    Heli -> "Helicopter"
    Gyro -> "Gyroplane"
    Hybrid -> "Hybrid Lift"
    Ornith -> "Ornithopter"
    Glider -> "Glider"
    Kite -> "Kite"
    FreeBalloon -> "Free Balloon"
    CaptiveBalloon -> "Captive Balloon"
    Airship -> "Airship"
    Parachute -> "Parachute"
    Rocket -> "Rocket"
    TetheredPwrAircraft -> "Tethered Powered Aircraft"
    GroundObstacle -> "Ground Obstacle"
    Other -> "Other"

readUAType :: Int -> Either String UAType
readUAType t
  | t >= 0 && t < 16 = Right $ toEnum t
  | otherwise = Left $ "failed to read UAType " ++ show t

-- | Operational status
data OpStatus
  = Undeclared | Ground | Airborne | Emergency | RemoteIDSystemFailure | OpStatusRsvd
  deriving (Eq, Read, Show)

instance Pretty OpStatus where
  pretty s = case s of
    RemoteIDSystemFailure -> "Remote ID System Failure"
    OpStatusRsvd -> "Reserved"
    _ -> viaShow s

data MsgType = BasicIDTy | Location | Auth | SelfIDTy | System | OperatorID | Pack
  deriving (Eq, Read, Show)

instance Pretty MsgType where
  pretty BasicIDTy = "BasicID"
  pretty SelfIDTy = "SelfID"
  pretty t = viaShow t

readMsgType :: Word8 -> Either String MsgType
readMsgType n = case n of
  0x0 -> Right BasicIDTy
  0x1 -> Right Location
  0x2 -> Right Auth
  0x3 -> Right SelfIDTy
  0x4 -> Right System
  0x5 -> Right OperatorID
  0xF -> Right Pack
  _   -> Left $ "failed to read msg type 0x" ++ showHex n ""

data MsgHdr = MsgHdr{ msgType :: MsgType, msgVer :: Integer }
  deriving (Eq, Read, Show)

instance Pretty MsgHdr where
  pretty (MsgHdr t v) = pretty t <+> "v" <> pretty v

mkMsgHdr :: Word8 -> Either String MsgHdr
mkMsgHdr w8 = MsgHdr <$> readMsgType (w8 `shiftR` 4) <*> pure (fromIntegral $ w8 .&. 0xF)

data MsgBdy
  = BasicID BasicIDMsg
  | SelfID
  deriving (Eq, Read, Show)

data BasicIDMsg = BasicIDMsg
  { basicIDType :: IDType
  , basicIDUA :: UAType
  , basicIDUASID :: UASID
  }
  deriving (Eq, Read, Show)

getBasicIDMsg :: Get BasicIDMsg
getBasicIDMsg = do
  w8     <- getWord8
  idType <- either fail return $ readIDType $ fromIntegral $ w8 `shiftR` 4
  uatype <- either fail return $ readUAType $ fromIntegral $ w8 .&. 0xF
  uasID  <- BS.takeWhile (/= 0x00) <$> getLazyByteString 20 <* getByteString 3
  return $ BasicIDMsg idType uatype uasID

data Msg = Msg{ msgHdr :: MsgHdr, msgBdy :: MsgBdy }
  deriving (Eq, Read, Show)

data IDType = IDTypeNone | SerialNum | CAARegID | UTMUUID | SpecificSessionID
  deriving (Eq, Enum, Read, Show)

readIDType :: Int -> Either String IDType
readIDType n
  | n >= 0 && n < 5 = Right $ toEnum n
  | otherwise = Left $ "failed to read ID type " ++ show n

type UASID = ByteString

-- | Horizontal accuracy. This is the NACp enumeration from ADS-B.
-- Value 12 was added for a more complete range for UAs. 95 % accuracy bound
-- (estimated position uncertainty).
data HorizAcc
  = GT10NM  -- ^ >=18.52 km (10 NM) or Unknown
  | LT10NM  -- ^ <18.52 km (10 NM)
  | LT4NM   -- ^ <7.408 km (4 NM)
  | LT2NM   -- ^ <3.704 km (2 NM)
  | LT1NM   -- ^ <1852 m (1 NM)
  | LT05NM  -- ^ <926 m (0.5 NM)
  | LT03NM  -- ^ <555.6 m (0.3 NM)
  | LT01NM  -- ^ <185.2 m (0.1 NM)
  | LT005NM -- ^ <92.6 m (0.05 NM)
  | LT30M   -- ^ <30 m
  | LT10M   -- ^ <10 m
  | LT3M    -- ^ <3 m
  | LT1M    -- ^ <1 m
  | HorizAccRsvd -- ^ Reserved
  deriving (Eq, Enum, Read, Show)

readHorizAcc :: Int -> Either String HorizAcc
readHorizAcc n
  | n >= 0 && n < 14 = Right $ toEnum n
  | n == 14 || n == 15 = Right HorizAccRsvd
  | otherwise = Left $ "horiz acc out of bounds " ++ show n
