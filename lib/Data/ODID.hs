{-# LANGUAGE OverloadedStrings #-}

-- | Open Drone ID
module Data.ODID
  ( ODID(..), readODID, writeODID
  , UA(..)
  ) where

import Data.Binary.Get
import Data.Bits
import Data.ByteString.Lazy (ByteString)
import Data.Word
import Numeric
import Prettyprinter

data ODID = ODID

readODID :: ByteString -> Either String ODID
readODID = undefined

writeODID :: ODID -> ByteString
writeODID = undefined

-- | Unmanned aircraft
data UA
  = None | Aeroplane | Heli | Gyro | Hybrid | Ornith | Glider | Kite | FreeBalloon
  | CaptiveBalloon | Airship | Parachute | Rocket | TetheredPwrAircraft | GroundObstacle
  | Other
  deriving (Eq, Read, Show)

instance Pretty UA where
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

-- | Operational status
data OpStatus
  = Undeclared | Ground | Airborne | Emergency | RemoteIDSystemFailure | OpStatusRsvd
  deriving (Eq, Read, Show)

instance Pretty OpStatus where
  pretty s = case s of
    RemoteIDSystemFailure -> "Remote ID System Failure"
    OpStatusRsvd -> "Reserved"
    _ -> viaShow s

data MsgType = BasicID | Location | Auth | SelfID | System | OperatorID | Pack
  deriving (Eq, Read, Show)

instance Pretty MsgType where
  pretty = viaShow

readMsgType :: Word8 -> Either String MsgType
readMsgType n = case n of
  0x0 -> Right BasicID
  0x1 -> Right Location
  0x2 -> Right Auth
  0x3 -> Right SelfID
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

data BasicIDMsgRaw = BasicIDMsgRaw Word8 ByteString
  deriving (Eq, Read, Show)

readBasicIDMsgRaw :: Get BasicIDMsgRaw
readBasicIDMsgRaw = liftA2 BasicIDMsgRaw getWord8 (getLazyByteString 20) <* getByteString 3
