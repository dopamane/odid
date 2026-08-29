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

readUAType :: (Eq a, Integral a, Num a, Show a) => a -> Either String UAType
readUAType t
  | t >= 0 && t < 16 = Right $ toEnum $ fromIntegral t
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
  idType <- either fail return $ readIDType $ w8 `shiftR` 4
  uatype <- either fail return $ readUAType $ w8 .&. 0xF
  uasID  <- BS.takeWhile (/= 0x00) <$> getLazyByteString 20 <* getByteString 3
  return $ BasicIDMsg idType uatype uasID

data Msg = Msg{ msgHdr :: MsgHdr, msgBdy :: MsgBdy }
  deriving (Eq, Read, Show)

data IDType = IDTypeNone | SerialNum | CAARegID | UTMUUID | SpecificSessionID
  deriving (Eq, Read, Show)

readIDType :: (Eq a, Num a, Show a) => a -> Either String IDType
readIDType n = case n of
  0 -> Right IDTypeNone
  1 -> Right SerialNum
  2 -> Right CAARegID
  3 -> Right UTMUUID
  4 -> Right SpecificSessionID
  _ -> Left $ "faild to read ID type " ++ show n

type UASID = ByteString
