{-# LANGUAGE OverloadedStrings #-}

-- | Open Drone ID
module Data.ODID
  ( Msg(..), MsgHdr(..), MsgType(..), msgTypes, MsgBdy(..)
  , UASID, UAType(..)
  , SysMsg(..), ClassType(..), OpLocSrc(..)
  ) where

import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Int
import Data.Word
import Numeric
import Prettyprinter

data Msg = Msg{msgHdr :: MsgHdr, msgBdy :: MsgBdy}
  deriving (Eq, Read, Show)

instance Binary Msg where
  get = get >>= \hdr -> Msg hdr <$> case msgType hdr of
    BasicIDTy -> getWord8 >>= \w8 ->
      BasicIDBdy
        <$> getIDType (w8 `shiftR` 4) <*> getUAType (w8 .&. 0xF)
        <*> getLazyByteString 20 <* getByteString 3
    Location -> return LocBdy
    Auth -> return AuthBdy
    SelfIDTy -> SelfIDBdy <$> get <*> getLazyByteString 23
    System -> SysBdy <$> get
    OperatorID -> OpIDBdy <$> getWord8 <*> getLazyByteString 20 <* getByteString 3
    Pack -> do
      sz <- getWord8
      nm <- getWord8
      PackBdy sz nm <$> replicateM (fromIntegral nm) get

  put (Msg hdr bdy) = put hdr <> case bdy of
    BasicIDBdy t ua uasid -> do
      let idTy = fromIntegral $ fromEnum t
          uaTy = fromIntegral $ fromEnum ua
      putWord8 $ idTy `shiftL` 4 .|. uaTy
      putLazyByteString $ uasid <> BS.replicate 3 0x00
    LocBdy -> undefined
    AuthBdy -> undefined
    SelfIDBdy ty desc -> putWord8 ty <> putLazyByteString desc
    SysBdy s -> put s
    OpIDBdy t i -> putWord8 t <> putLazyByteString (i <> BS.replicate 3 0x00)
    PackBdy sz nm ms -> putWord8 sz <> putWord8 nm <> foldMap put ms

instance Pretty Msg where
  pretty (Msg hdr bdy) = vsep [pretty hdr, pretty bdy]

-- | Unmanned aircraft
data UAType
  = None | Aeroplane | Heli | Gyro | Hybrid | Ornith | Glider | Kite | FreeBalloon
  | CaptiveBalloon | Airship | Parachute | Rocket | TetheredPwrAircraft | GroundObstacle
  | Other
  deriving (Bounded, Eq, Enum, Read, Show)

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

getUAType :: Word8 -> Get UAType
getUAType t | t < 16 = return $ toEnum $ fromIntegral t
            | otherwise = fail $ "cannot read UAType " ++ show t

-- | Operational status
data OpStatus = Undeclared | Ground | Airborne | Emergency
  | RemoteIDSystemFailure | OpStatusRsvd Word8
  deriving (Eq, Read, Show)

instance Pretty OpStatus where
  pretty s = case s of
    RemoteIDSystemFailure -> "Remote ID System Failure"
    OpStatusRsvd r -> "Reserved" <+> pretty r
    _ -> viaShow s

data MsgType = BasicIDTy | Location | Auth | SelfIDTy | System | OperatorID | Pack
  deriving (Eq, Read, Show)

msgTypes :: [MsgType]
msgTypes = [BasicIDTy, Location, Auth, SelfIDTy, System, OperatorID, Pack]

instance Pretty MsgType where
  pretty BasicIDTy = "BasicID"
  pretty SelfIDTy = "SelfID"
  pretty t = viaShow t

data MsgHdr = MsgHdr{msgVer :: Word8, msgType :: MsgType}
  deriving (Eq, Read, Show)

instance Binary MsgHdr where
  get = do
    w8 <- getWord8
    MsgHdr (w8 .&. 0xF) <$> case w8 `shiftR` 4 of
      0x0 -> return BasicIDTy
      0x1 -> return Location
      0x2 -> return Auth
      0x3 -> return SelfIDTy
      0x4 -> return System
      0x5 -> return OperatorID
      0xF -> return Pack
      n   -> fail $ "cannot read msg type 0x" ++ showHex n ""

  put (MsgHdr v t) = putWord8 $ tNyb `shiftL` 4 .|. v
    where
      tNyb = case t of
        BasicIDTy  -> 0x0
        Location   -> 0x1
        Auth       -> 0x2
        SelfIDTy   -> 0x3
        System     -> 0x4
        OperatorID -> 0x5
        Pack       -> 0xF

instance Pretty MsgHdr where
  pretty (MsgHdr v t) = "v" <> pretty v <+> pretty t

data MsgBdy = BasicIDBdy IDType UAType UASID | LocBdy | AuthBdy | SelfIDBdy Word8 ByteString
  | SysBdy SysMsg | OpIDBdy Word8 ByteString | PackBdy Word8 Word8 [Msg]
  deriving (Eq, Read, Show)

instance Pretty MsgBdy where
  pretty = viaShow

type UASID = ByteString

data IDType = IDTypeNone | SerialNum | CAARegID | UTMUUID | SpecificSessionID
  deriving (Bounded, Eq, Enum, Read, Show)

getIDType :: Word8 -> Get IDType
getIDType n | n < 5 = return $ toEnum $ fromIntegral n
            | otherwise = fail $ "cannot read ID type " ++ show n

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

instance Pretty HorizAcc where
  pretty a = case a of
    GT10NM  -> ">=18.52 km (10 NM) or Unknown"
    LT10NM  -> "<18.52 km (10 NM)"
    LT4NM   -> "<7.408 km (4 NM)"
    LT2NM   -> "<3.704 km (2 NM)"
    LT1NM   -> "<1852 m (1 NM)"
    LT05NM  -> "<926 m (0.5 NM)"
    LT03NM  -> "<555.6 m (0.3 NM)"
    LT01NM  -> "<185.2 m (0.1 NM)"
    LT005NM -> "<92.6 m (0.05 NM)"
    LT30M   -> "<30 m"
    LT10M   -> "<10 m"
    LT3M    -> "<3 m"
    LT1M    -> "<1 m"
    HorizAccRsvd -> "Reserved"

readHorizAcc :: Int -> Either String HorizAcc
readHorizAcc n
  | n >= 0 && n < 14 = Right $ toEnum n
  | n == 14 || n == 15 = Right HorizAccRsvd
  | otherwise = Left $ "horiz acc out of bounds " ++ show n

data EUClassType = Undefined | Open | Specific | Certified | EUClassTypeRsvd

-- | Vertical Accuracy. This is the GVA enumeration from ADS-B. Values 4–6 were added for
-- UAs. 95 % accuracy bound.
data VertAcc
  = VertAccGTE150M -- ^ >=150 m or Unknown
  | VertAccLT150M-- ^ <150 m
  | VertAccLT45M -- ^ <45 m
  | VertAccLT25M -- ^ <25 m
  | VertAccLT10M -- ^ <10 m
  | VertAccLT3M -- ^ <3 m
  | VertAccLT1M -- ^ <1 m
  | VertAccRsvd

instance Pretty VertAcc where
  pretty a = case a of
    VertAccGTE150M -> ">=150 m"
    VertAccLT150M  -> "<150 m"
    VertAccLT45M   -> "<45 m"
    VertAccLT25M   -> "<25 m"
    VertAccLT10M   -> "<10 m"
    VertAccLT3M    -> "<3 m"
    VertAccLT1M    -> "<1 m"
    VertAccRsvd    -> "Reserved"

-- | Speed Accuracy. This is the same enumeration scale and values from ADS-B NACv.
-- 95 % accuracy bound.
data SpeedAcc = GTE10MS | LT10MS | LT3MS | LT1MS | LT03MS | SpeedAccRsvd

instance Pretty SpeedAcc where
  pretty a = case a of
    GTE10MS -> ">=10 m/s or Unknown"
    LT10MS -> "<10 m/s"
    LT3MS -> "<3 m/s"
    LT1MS -> "<1 m/s"
    LT03MS -> "<0.3 m/s"
    SpeedAccRsvd -> "Reserved"

-- | Location message
data LocMsg = LocMsg
  { locStatusFlags :: Word8, locTrackDir :: Word8, locSpeed :: Word8
  , locVertSpeed :: Word8, locLat :: Word32, locLon :: Word32
  , locPresAlt :: Word16, locGeoAlt :: Word16, locHeight :: Word16
  , locVertHorzAcc :: Word8, locBaroAltAccSpeedAcc :: Word8
  , locTimestamp :: Word16, locTimestampAcc :: Word8
  }
  deriving (Eq, Read, Show)

instance Binary LocMsg where
  get = undefined
  put = undefined

data ClassType = ClassTypeUndeclared | EuroUnion | ClassTypeRsvd Word8
  deriving (Eq, Read, Show)

instance Pretty ClassType where
  pretty ClassTypeUndeclared = "Undeclared"
  pretty EuroUnion = "European Union"
  pretty (ClassTypeRsvd n) = "Reserved" <+> pretty n

-- | Operator location source type
data OpLocSrc = Takeoff | Dynamic | Fixed
  deriving (Enum, Eq, Read, Show)

instance Pretty OpLocSrc where
  pretty = viaShow

data SysMsg = SysMsg
  { sysClassType :: ClassType, sysOpSrcType :: OpLocSrc, sysOpLat :: Double
  , sysOpLon :: Double, sysArCnt :: Word16, sysArRad :: Integer, sysArCeil :: Word16
  , sysArFloor :: Word16, sysUAClass :: Word8, sysOpAlt :: Word16, sysTimestamp :: Word32
  }
  deriving (Eq, Read, Show)

instance Binary SysMsg where
  get = do
   flags <- getWord8
   let classType = case 0x7 .&. flags `shiftR` 2 of
         0 -> ClassTypeUndeclared
         1 -> EuroUnion
         n -> ClassTypeRsvd n
       srcType = case 0x3 .&. flags of
         0 -> Takeoff
         1 -> Dynamic
         _ -> Fixed
   SysMsg classType srcType
     <$> fmap ((/ 10 ^ seven) . fromIntegral) getInt32le
     <*> fmap ((/ 10 ^ seven) . fromIntegral) getInt32le
     <*> getWord16le
     <*> fmap ((* 10) . fromIntegral) getWord8
     <*> getWord16le
     <*> getWord16le
     <*> getWord8
     <*> getWord16le
     <*> getWord32le
     <*  getWord8

  put m@SysMsg{sysClassType=ct,sysOpSrcType=opSrc} = do
    let classTy = case ct of
                    ClassTypeUndeclared -> 0
                    EuroUnion -> 1
                    ClassTypeRsvd r -> r
        flags = classTy `shiftL` 2 .|. fromIntegral (fromEnum opSrc)
    putWord8 flags
    putInt32le $ floor $ sysOpLat m * 10 ^ seven
    putInt32le $ floor $ sysOpLon m * 10 ^ seven
    putWord16le $ sysArCnt m
    putWord8 $ fromIntegral $ sysArRad m `div` 10

instance Pretty SysMsg where
  pretty = viaShow

seven :: Int
seven = 7
