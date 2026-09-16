{-# LANGUAGE OverloadedStrings #-}

-- | Open Drone ID
module Data.ODID
  ( Msg(..), MsgHdr(..), MsgType(..), msgTypes, MsgBdy(..)
  , UASID, UAType(..)
  , SysMsg(..), ClassType(..), OpLocSrc(..)
  , AuthMsg(..), LocMsg(..)
  ) where

import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Function
import Data.Int
import Data.Word
import Numeric
import Prettyprinter

data Msg = Msg{msgHdr :: MsgHdr, msgBdy :: MsgBdy}
  deriving (Eq, Read, Show)

instance Binary Msg where
  get = get >>= \hdr -> Msg hdr <$> case msgType hdr of
    BasicIDTy -> getWord8 >>= \w8 ->
      BasicIDBdy <$> getIDType (w8 `shiftR` 4) <*> getUAType (w8 .&. 0xF)
                 <*> getLazyByteString 20 <*> getLazyByteString 3
    Location -> LocBdy <$> get
    Auth -> AuthBdy <$> get
    SelfIDTy -> SelfIDBdy <$> get <*> getLazyByteString 23
    System -> SysBdy <$> get
    OperatorID -> OpIDBdy <$> getWord8 <*> getLazyByteString 20 <* getByteString 3
    Pack -> getWord8 >>= \sz -> getWord8 >>= \nm ->
      PackBdy sz nm <$> replicateM (fromIntegral nm) get

  put (Msg hdr bdy) = put hdr <> case bdy of
    BasicIDBdy t ua uasid rsvd -> do
      putWord8 $ fromIntegral $ fromEnum t `shiftL` 4 .|. fromEnum ua
      putLazyByteString $ uasid <> rsvd
    LocBdy l -> put l
    AuthBdy a -> put a
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
  pretty t = case t of
    BasicIDTy -> "Basic ID"
    SelfIDTy -> "Self ID"
    OperatorID -> "Operator ID"
    _ -> viaShow t

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

data MsgBdy = BasicIDBdy IDType UAType UASID ByteString | LocBdy LocMsg | AuthBdy AuthMsg
  | SelfIDBdy Word8 ByteString | SysBdy SysMsg | OpIDBdy Word8 ByteString
  | PackBdy Word8 Word8 [Msg]
  deriving (Eq, Read, Show)

instance Pretty MsgBdy where
  pretty m = case m of
    BasicIDBdy idTy uaTy uasid _rsvd -> vsep ["ID Type:" <+> pretty idTy
      , "UA Type:" <+> pretty uaTy, "UASID:" <+> pretty (BSC.unpack uasid)]
    LocBdy l -> pretty l
    AuthBdy a -> pretty a
    SelfIDBdy ty desc -> vsep [pretty ty, pretty $ BSC.unpack desc]
    SysBdy s -> pretty s
    OpIDBdy t i -> vsep [pretty t, pretty $ BSC.unpack i]
    PackBdy sz nm ms -> vsep ["Size=" <> pretty sz <+> "Cnt=" <> pretty nm
      , indent 2 $ vsep $ pretty <$> ms]

type UASID = ByteString

data IDType = IDTypeNone | SerialNum | CAARegID | UTMUUID | SpecificSessionID
  deriving (Bounded, Eq, Enum, Read, Show)

instance Pretty IDType where
  pretty t = case t of
    IDTypeNone -> "None"
    SerialNum -> "Serial Number (ANSI/CTA-2063-A)"
    CAARegID -> "CAA Assigned Registration ID"
    UTMUUID -> "UTM Assigned UUID"
    SpecificSessionID -> "Specific Session ID"

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
  | HorizAccRsvd Word8 -- ^ Reserved
  deriving (Eq, Read, Show)

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
    HorizAccRsvd n -> "Reserved" <+> pretty n

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
  | VertAccRsvd Word8
  deriving (Eq, Read, Show)

instance Pretty VertAcc where
  pretty a = case a of
    VertAccGTE150M -> ">=150 m"
    VertAccLT150M  -> "<150 m"
    VertAccLT45M   -> "<45 m"
    VertAccLT25M   -> "<25 m"
    VertAccLT10M   -> "<10 m"
    VertAccLT3M    -> "<3 m"
    VertAccLT1M    -> "<1 m"
    VertAccRsvd n  -> "Reserved" <+> pretty n

-- | Speed Accuracy. This is the same enumeration scale and values from ADS-B NACv.
-- 95 % accuracy bound.
data SpeedAcc = GTE10MS | LT10MS | LT3MS | LT1MS | LT03MS | SpeedAccRsvd Word8
  deriving (Eq, Read, Show)

instance Pretty SpeedAcc where
  pretty a = case a of
    GTE10MS -> ">=10 m/s or Unknown"
    LT10MS -> "<10 m/s"
    LT3MS -> "<3 m/s"
    LT1MS -> "<1 m/s"
    LT03MS -> "<0.3 m/s"
    SpeedAccRsvd n -> "Reserved" <+> pretty n

data HeightType = AboveTakeoff | AGL
  deriving (Eq, Read, Show)

-- | Location message
data LocMsg = LocMsg
  { locOpStatus :: OpStatus, locFlagsRsvd :: Bool, locFlagsHeightType :: HeightType
  , locFlagsDir :: Bool, locFlagsMult :: Bool, locTrackDir :: Integer, locSpeed :: Double
  , locVertSpeed :: Double, locLat :: Double, locLon :: Double
  , locPresAlt :: Double, locGeoAlt :: Double, locHeight :: Double
  , locVertAcc :: VertAcc, locHorzAcc :: HorizAcc
  , locBaroAltAccSpeedAcc :: Word8, locSpeedAcc :: SpeedAcc
  , locTimestamp :: Double -- ^ seconds after the hour
  , locTStampAccRsvd :: Word8, locTStampAcc :: Double -- ^ timestamp accuracy seconds, 0.1 s res
  , locRsvd :: Word8
  }
  deriving (Eq, Read, Show)

instance Binary LocMsg where
  get = do
    w8 <- getWord8
    let opStatus = case w8 `shiftR` 4 of
          0 -> Undeclared
          1 -> Ground
          2 -> Airborne
          3 -> Emergency
          4 -> RemoteIDSystemFailure
          n -> OpStatusRsvd n
        flgsRsvd = testBit w8 3
        ht = if testBit w8 2 then AGL else AboveTakeoff
        dir = testBit w8 1
        mul = testBit w8 0
    trackDir <- applyWhen dir (+ 180) . fromIntegral <$> getWord8
    speed <- decSpeed mul . fromIntegral <$> getWord8
    vertSpeed <- (* 0.5) . fromIntegral <$> getInt8
    lat <- decLatLon <$> getInt32le
    lon <- decLatLon <$> getInt32le
    palt <- decodeAlt <$> getWord16le
    galt <- decodeAlt <$> getWord16le
    hgt  <- decodeAlt <$> getWord16le
    vhacc <- getWord8
    let vacc = case vhacc `shiftR` 4 of
          0 -> VertAccGTE150M
          1 -> VertAccLT150M
          2 -> VertAccLT45M
          3 -> VertAccLT25M
          4 -> VertAccLT10M
          5 -> VertAccLT3M
          6 -> VertAccLT1M
          r -> VertAccRsvd r
        hacc = undefined
    bacc <- getWord8
    let speedAcc = case bacc .&. 0xF of
          0 -> GTE10MS
          1 -> LT10MS
          2 -> LT3MS
          3 -> LT1MS
          4 -> LT03MS
          n -> SpeedAccRsvd n
    tstmp <- (* 0.1) . fromIntegral <$> getWord16le
    tstmprsvdacc <- getWord8
    let tstmpacc = fromIntegral (tstmprsvdacc .&. 0xF) * 0.1
    LocMsg opStatus flgsRsvd ht dir mul trackDir speed vertSpeed lat
      lon palt galt hgt vacc hacc bacc speedAcc tstmp (tstmprsvdacc `shiftR` 4) tstmpacc <$> getWord8
    where
      decSpeed mul s = if mul then s * 0.25 else s * 0.75 + 255 * 0.25

  put l = do
    let opStatus = case locOpStatus l of
          Undeclared -> 0
          Ground -> 1
          Airborne -> 2
          Emergency -> 3
          RemoteIDSystemFailure -> 4
          OpStatusRsvd n -> n
    putWord8 $ opStatus `shiftL` 4 .|. undefined
    putWord8 $ fromIntegral $ applyWhen (locTrackDir l >= 180) (subtract 180) $ locTrackDir l
    putWord8 undefined
    putInt8 $ truncate $ locVertSpeed l * 2
    putInt32le $ encLatLon $ locLat l
    putInt32le $ encLatLon $ locLon l
    putWord16le $ encodeAlt $ locPresAlt l
    putWord16le $ encodeAlt $ locGeoAlt l
    putWord16le $ encodeAlt $ locHeight l
    let vacc = case locVertAcc l of
          VertAccGTE150M -> 0
          VertAccLT150M  -> 1
          VertAccLT45M   -> 2
          VertAccLT25M   -> 3
          VertAccLT10M   -> 4
          VertAccLT3M    -> 5
          VertAccLT1M    -> 6
          VertAccRsvd r  -> r
        hAcc = undefined
    putWord8 $ vacc `shiftL` 4 .|. hAcc
    putWord8 undefined
    putWord16le $ truncate $ locTimestamp l / 0.1
    putWord8 $ locTStampAccRsvd l `shiftL` 4 .|. truncate (locTStampAcc l / 0.1)
    putWord8 $ locRsvd l

instance Pretty LocMsg where
  pretty = viaShow

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
  , sysOpLon :: Double, sysArCnt :: Word16, sysArRad :: Integer, sysArCeil :: Double
  , sysArFloor :: Double, sysUAClass :: Word8, sysOpAlt :: Double, sysTimestamp :: Word32
  , sysRsvd :: Word8
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
     <$> fmap decLatLon getInt32le
     <*> fmap decLatLon getInt32le
     <*> getWord16le
     <*> fmap ((* 10) . fromIntegral) getWord8
     <*> fmap decodeAlt getWord16le
     <*> fmap decodeAlt getWord16le
     <*> getWord8
     <*> fmap decodeAlt getWord16le
     <*> getWord32le
     <*> getWord8

  put m = do
    putWord8 $ classTy `shiftL` 2 .|. fromIntegral (fromEnum $ sysOpSrcType m)
    putInt32le $ encLatLon $ sysOpLat m
    putInt32le $ encLatLon $ sysOpLon m
    putWord16le $ sysArCnt m
    putWord8 $ fromIntegral $ sysArRad m `div` 10
    putWord16le $ encodeAlt $ sysArCeil m
    putWord16le $ encodeAlt $ sysArFloor m
    putWord8 undefined
    putWord16le $ encodeAlt $ sysOpAlt m
    putWord32le undefined
    putWord8 $ sysRsvd m
    where
      classTy = case sysClassType m of
        ClassTypeUndeclared -> 0
        EuroUnion -> 1
        ClassTypeRsvd r -> r

instance Pretty SysMsg where
  pretty = viaShow

data AuthMsg = AuthMsg
  deriving (Eq, Read, Show)

instance Binary AuthMsg where
  get = undefined
  put = undefined

instance Pretty AuthMsg where
  pretty = viaShow

encodeAlt :: Double -> Word16
encodeAlt x = truncate $ (x + 1000) * 2

decodeAlt :: Word16 -> Double
decodeAlt x = fromIntegral x * 0.5 - 1000

encLatLon :: Double -> Int32
encLatLon = truncate . (* latLonMult)

decLatLon :: Int32 -> Double
decLatLon = (/ latLonMult) . fromIntegral

latLonMult :: Double
latLonMult = 10 ^ (7 :: Int)
