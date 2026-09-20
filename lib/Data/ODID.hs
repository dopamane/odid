{-# LANGUAGE OverloadedStrings #-}

-- | Open Drone ID
module Data.ODID
  ( Msg(..), MsgHdr(..), MsgType(..), msgTypes, MsgBdy(..)
  , IDType(..), UASID, UAType(..)
  , SysMsg(..), ClassType(..), ClassCat(..), OpLocSrc(..)
  , AuthMsg(..), LocMsg(..)
  ) where

import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Function
import Data.Int
import Foreign
import Numeric
import Prettyprinter

data Msg = Msg{msgHdr :: MsgHdr, msgBdy :: MsgBdy}
  deriving (Eq, Read, Show)

instance Binary Msg where
  get = get >>= \hdr -> Msg hdr <$> case msgType hdr of
    BasicIDTy -> get >>= \b -> BasicIDBdy (getIDType $ b `shiftR` 4)
      (toEnum $ fromIntegral $ b .&. 0xF) <$> getLazyByteString 20 <*> getLazyByteString 3
    Location -> LocBdy <$> get
    Auth -> AuthBdy <$> get
    SelfIDTy -> SelfIDBdy <$> get <*> getLazyByteString 23
    System -> SysBdy <$> get
    OperatorID -> OpIDBdy <$> get <*> getLazyByteString 20 <*> getLazyByteString 3
    Pack -> get >>= \sz -> get >>= \nm ->
      PackBdy sz nm <$> replicateM (fromIntegral nm) get
    where
      getIDType t = case t of
        0 -> IDTypeNone
        1 -> SerialNum
        2 -> CAARegID
        3 -> UTMUUID
        4 -> SpecificSessionID
        n -> IDTypeRsvd n

  put (Msg hdr bdy) = put hdr <> case bdy of
    BasicIDBdy t ua uasid rsvd -> do
      let ty = case t of
            IDTypeNone -> 0
            SerialNum -> 1
            CAARegID -> 2
            UTMUUID -> 3
            SpecificSessionID -> 4
            IDTypeRsvd r -> r
      putWord8 $ ty `shiftL` 4 .|. fromIntegral (fromEnum ua)
      putLazyByteString $ uasid <> rsvd
    LocBdy l -> put l
    AuthBdy a -> put a
    SelfIDBdy ty desc -> put ty <> putLazyByteString desc
    SysBdy s -> put s
    OpIDBdy t i r -> put t <> putLazyByteString (i <> r)
    PackBdy sz nm ms -> put sz <> put nm <> foldMap put ms

instance Pretty Msg where
  pretty (Msg hdr bdy) = vsep [pretty hdr, pretty bdy]

-- | Unmanned aircraft
data UAType = None | Aeroplane | Heli | Gyro | Hybrid | Ornith | Glider | Kite
  | FreeBalloon | CaptiveBalloon | Airship | Parachute | Rocket
  | TetheredPwrAircraft | GroundObstacle | Other
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
  get = get >>= \b -> MsgHdr (b .&. 0xF) <$> case b `shiftR` 4 of
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
  | SelfIDBdy Word8 ByteString | SysBdy SysMsg | OpIDBdy Word8 ByteString ByteString
  | PackBdy Word8 Word8 [Msg]
  deriving (Eq, Read, Show)

instance Pretty MsgBdy where
  pretty m = case m of
    BasicIDBdy idTy uaTy uasid _rsvd -> vsep ["ID Type:" <+> pretty idTy
      , "UA Type:" <+> pretty uaTy, "UASID:" <+> pretty (BSC.unpack uasid)]
    LocBdy l -> pretty l
    AuthBdy a -> pretty a
    SelfIDBdy ty desc -> vsep ["Type:" <+> pretty ty, "Desc:" <+> pretty (BSC.unpack desc)]
    SysBdy s -> pretty s
    OpIDBdy t i r -> vsep ["Type:" <+> pretty t, "ID:" <+> pretty (BSC.unpack i)
      , "Rsvd:" <+> pretty (BSC.unpack r)]
    PackBdy sz nm ms -> vsep ["Size=" <> pretty sz <+> "Cnt=" <> pretty nm
      , indent 2 $ vsep $ pretty <$> ms]

type UASID = ByteString

data IDType = IDTypeNone | SerialNum | CAARegID | UTMUUID | SpecificSessionID
  | IDTypeRsvd Word8
  deriving (Eq, Read, Show)

instance Pretty IDType where
  pretty t = case t of
    IDTypeNone -> "None"
    SerialNum -> "Serial Number (ANSI/CTA-2063-A)"
    CAARegID -> "CAA Assigned Registration ID"
    UTMUUID -> "UTM Assigned UUID"
    SpecificSessionID -> "Specific Session ID"
    IDTypeRsvd r -> "Reserved" <+> pretty r

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

data ClassCat = Undefined | Open | Specific | Certified | ClassCatRsvd Word8
  deriving (Eq, Read, Show)

instance Pretty ClassCat where
  pretty (ClassCatRsvd n) = "Reserved" <+> pretty n
  pretty c = viaShow c

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
    VertAccGTE150M -> ">=150 m or Unknown"
    VertAccLT150M  -> "<150 m"
    VertAccLT45M   -> "<45 m"
    VertAccLT25M   -> "<25 m"
    VertAccLT10M   -> "<10 m"
    VertAccLT3M    -> "<3 m"
    VertAccLT1M    -> "<1 m"
    VertAccRsvd n  -> "Reserved" <+> pretty n

readVertAcc :: Word8 -> VertAcc
readVertAcc n = case n of
  0 -> VertAccGTE150M
  1 -> VertAccLT150M
  2 -> VertAccLT45M
  3 -> VertAccLT25M
  4 -> VertAccLT10M
  5 -> VertAccLT3M
  6 -> VertAccLT1M
  _ -> VertAccRsvd n

writeVertAcc :: VertAcc -> Word8
writeVertAcc vacc = case vacc of
  VertAccGTE150M -> 0
  VertAccLT150M  -> 1
  VertAccLT45M   -> 2
  VertAccLT25M   -> 3
  VertAccLT10M   -> 4
  VertAccLT3M    -> 5
  VertAccLT1M    -> 6
  VertAccRsvd r  -> r

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
  deriving (Bounded, Enum, Eq, Read, Show)

instance Pretty HeightType where
  pretty AboveTakeoff = "Above Takeoff"
  pretty AGL = "AGL"

-- | Location message
data LocMsg = LocMsg
  { locOpStatus :: OpStatus, locFlagsRsvd :: Bool, locHeightType :: HeightType
  , locFlagsDir :: Bool, locFlagsMult :: Bool, locTrackDir :: Integer, locSpeed :: Double
  , locVertSpeed :: Double, locLat :: Double, locLon :: Double
  , locPresAlt :: Double, locGeoAlt :: Double, locHeight :: Double
  , locVertAcc :: VertAcc, locHorzAcc :: HorizAcc
  , locBaroAltAcc :: VertAcc, locSpeedAcc :: SpeedAcc
  , locTimestamp :: Double -- ^ seconds after the hour
  , locTStampAccRsvd :: Word8, locTStampAcc :: Double -- ^ timestamp accuracy seconds, 0.1 s res
  , locRsvd :: Word8
  }
  deriving (Eq, Read, Show)

instance Binary LocMsg where
  get = getWord8 >>= \b -> do
    let opStatus = case b `shiftR` 4 of
          0 -> Undeclared
          1 -> Ground
          2 -> Airborne
          3 -> Emergency
          4 -> RemoteIDSystemFailure
          n -> OpStatusRsvd n
        flgsRsvd = testBit b 3
        ht = if testBit b 2 then AGL else AboveTakeoff
        dir = testBit b 1
        mul = testBit b 0
    trackDir <- applyWhen dir (+ 180) . fromIntegral <$> getWord8
    speed <- decSpeed mul . fromIntegral <$> getWord8
    vertSpeed <- (* 0.5) . fromIntegral <$> getInt8
    lat <- decLatLon <$> getInt32le
    lon <- decLatLon <$> getInt32le
    palt <- decodeAlt <$> getWord16le
    galt <- decodeAlt <$> getWord16le
    hgt  <- decodeAlt <$> getWord16le
    vhacc <- getWord8
    let vacc = readVertAcc $ vhacc `shiftR` 4
        hacc = case vhacc .&. 0xF of
          0  -> GT10NM
          1  -> LT10NM
          2  -> LT4NM
          3  -> LT2NM
          4  -> LT1NM
          5  -> LT05NM
          6  -> LT03NM
          7  -> LT01NM
          8  -> LT005NM
          9  -> LT30M
          10 -> LT10M
          11 -> LT3M
          12 -> LT1M
          n  -> HorizAccRsvd n
    bacc <- getWord8
    let baroAcc = readVertAcc $ bacc `shiftR` 4
        speedAcc = case bacc .&. 0xF of
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
      lon palt galt hgt vacc hacc baroAcc speedAcc tstmp
      (tstmprsvdacc `shiftR` 4) tstmpacc <$> getWord8
    where
      decSpeed mul s = if mul then s * 0.25 else s * 0.75 + 255 * 0.25

  put l = do
    putWord8 $ opStatus `shiftL` 4 .|. rsvdFlag `shiftL` 3 .|. htTy `shiftL` 2 .|.
      ewDir `shiftL` 1 .|. spMult
    putWord8 $ fromIntegral $ applyWhen (locTrackDir l >= 180) (subtract 180) $ locTrackDir l
    putWord8 $ round $ encSpeed $ locSpeed l
    putInt8 $ round $ locVertSpeed l * 2
    putInt32le $ encLatLon $ locLat l
    putInt32le $ encLatLon $ locLon l
    putWord16le $ encodeAlt $ locPresAlt l
    putWord16le $ encodeAlt $ locGeoAlt l
    putWord16le $ encodeAlt $ locHeight l
    putWord8 $ writeVertAcc (locVertAcc l) `shiftL` 4 .|. hAcc
    putWord8 $ writeVertAcc (locBaroAltAcc l) `shiftL` 4 .|. spAcc
    putWord16le $ round $ locTimestamp l / 0.1
    putWord8 $ locTStampAccRsvd l `shiftL` 4 .|. round (locTStampAcc l / 0.1)
    putWord8 $ locRsvd l
    where
      opStatus = case locOpStatus l of
        Undeclared -> 0
        Ground -> 1
        Airborne -> 2
        Emergency -> 3
        RemoteIDSystemFailure -> 4
        OpStatusRsvd n -> n
      rsvdFlag = fromBool $ locFlagsRsvd l
      htTy = fromIntegral $ fromEnum $ locHeightType l
      ewDir = fromBool $ locFlagsDir l
      spMult = fromBool $ locFlagsMult l
      hAcc = case locHorzAcc l of
        GT10NM -> 0
        LT10NM -> 1
        LT4NM -> 2
        LT2NM -> 3
        LT1NM -> 4
        LT05NM -> 5
        LT03NM -> 6
        LT01NM -> 7
        LT005NM -> 8
        LT30M -> 9
        LT10M -> 10
        LT3M -> 11
        LT1M -> 12
        HorizAccRsvd r -> r
      spAcc = case locSpeedAcc l of
        GTE10MS -> 0
        LT10MS -> 1
        LT3MS -> 2
        LT1MS -> 3
        LT03MS -> 4
        SpeedAccRsvd r -> r

instance Pretty LocMsg where
  pretty l = vsep
    ["Operational Status:" <+> pretty (locOpStatus l)
    , "Flags", indent 2 $ vsep
      [ "Reserved:" <+> if locFlagsRsvd l then "1" else "0"
      , "Height type:" <+> pretty (locHeightType l)
      , "E/W Dir Seg:" <+> if locFlagsDir l then ">=180" else "<180"
      , "Speed mult:" <+> if locFlagsMult l then "x0.75" else "x0.25"
      ]
    , "Track dir:" <+> pretty (locTrackDir l) <+> "deg"
    , "Speed:" <+> pretty (locSpeed l) <+> "m/s"
    , "Vert Speed:" <+> pretty (locVertSpeed l) <+> "m/s"
    , "Latitude:" <+> pretty (locLat l) <+> "deg"
    , "Longitude:" <+> pretty (locLon l) <+> "deg"
    , "Pressure Alt:" <+> pretty (locPresAlt l) <+> "m"
    , "Geodetic Alt:" <+> pretty (locGeoAlt l) <+> "m"
    , "Height:" <+> pretty (locHeight l) <+> "m"
    , "Vert Acc:" <+> pretty (locVertAcc l)
    , "Horz Acc:" <+> pretty (locHorzAcc l)
    , "Baro Alt Acc:" <+> pretty (locBaroAltAcc l)
    , "Speed Acc:" <+> pretty (locSpeedAcc l)
    , "Timestamp:" <+> pretty (locTimestamp l)
    , "Reserved:" <+> pretty (locTStampAccRsvd l)
    , "TStamp Acc:" <+> pretty (locTStampAcc l) <+> "s"
    , "Reserved:" <+> pretty (locRsvd l)
    ]

data ClassType = ClassTypeUndeclared | EuroUnion | ClassTypeRsvd Word8
  deriving (Eq, Read, Show)

instance Pretty ClassType where
  pretty ClassTypeUndeclared = "Undeclared"
  pretty EuroUnion = "European Union"
  pretty (ClassTypeRsvd n) = "Reserved" <+> pretty n

-- | Operator location source type
data OpLocSrc = Takeoff | Dynamic | Fixed
  deriving (Bounded, Enum, Eq, Read, Show)

instance Pretty OpLocSrc where
  pretty = viaShow

data SysMsg = SysMsg
  { sysClassType :: ClassType, sysOpSrcType :: OpLocSrc, sysOpLat :: Double
  , sysOpLon :: Double, sysArCnt :: Word16, sysArRad :: Integer, sysArCeil :: Double
  , sysArFloor :: Double, sysClassCat :: ClassCat, sysClassClass :: Word8
  , sysOpAlt :: Double, sysTimestamp :: Word32, sysRsvd :: Word8
  }
  deriving (Eq, Read, Show)

instance Binary SysMsg where
  get = getWord8 >>= \flags -> do
    let classType = case 0x7 .&. flags `shiftR` 2 of
          0 -> ClassTypeUndeclared
          1 -> EuroUnion
          n -> ClassTypeRsvd n
        srcType = if testBit flags 1 then Fixed else toEnum $ fromBool $ testBit flags 0
    opLat <- decLatLon <$> getInt32le
    opLon <- decLatLon <$> getInt32le
    arCnt <- getWord16le
    arRad <- (* 10) . fromIntegral <$> getWord8
    arCeil <- decodeAlt <$> getWord16le
    arFlor <- decodeAlt <$> getWord16le
    uaClass <- getWord8
    let classCat = case uaClass `shiftR` 4 of
          0 -> Undefined
          1 -> Open
          2 -> Specific
          3 -> Certified
          r -> ClassCatRsvd r
    SysMsg classType srcType opLat opLon arCnt arRad arCeil arFlor classCat
      (uaClass .&. 0xF) <$> fmap decodeAlt getWord16le <*> getWord32le <*> getWord8

  put m = do
    putWord8 $ classTy `shiftL` 2 .|. fromIntegral (fromEnum $ sysOpSrcType m)
    putInt32le $ encLatLon $ sysOpLat m
    putInt32le $ encLatLon $ sysOpLon m
    putWord16le $ sysArCnt m
    putWord8 $ fromIntegral $ sysArRad m `div` 10
    putWord16le $ encodeAlt $ sysArCeil m
    putWord16le $ encodeAlt $ sysArFloor m
    putWord8 $ classCat `shiftL` 4 .|. sysClassClass m .&. 0xF
    putWord16le $ encodeAlt $ sysOpAlt m
    putWord32le $ sysTimestamp m
    putWord8 $ sysRsvd m
    where
      classTy = case sysClassType m of
        ClassTypeUndeclared -> 0
        EuroUnion -> 1
        ClassTypeRsvd r -> r
      classCat = case sysClassCat m of
        Undefined -> 0
        Open -> 1
        Specific -> 2
        Certified -> 3
        ClassCatRsvd r -> r

instance Pretty SysMsg where
  pretty s = vsep
    [ "Class:" <+> pretty (sysClassType s)
    , "Op Src:" <+> pretty (sysOpSrcType s)
    , "Op Lat:" <+> pretty (sysOpLat s) <+> "deg"
    , "Op Lon:" <+> pretty (sysOpLon s) <+> "deg"
    , "Area Cnt:" <+> pretty (sysArCnt s)
    , "Area Rad:" <+> pretty (sysArRad s) <+> "m"
    , "Area Ceil:" <+> pretty (sysArCeil s) <+> "m"
    , "Area Floor:" <+> pretty (sysArFloor s) <+> "m"
    , "UA Category:" <+> pretty (sysClassCat s)
    , "UA Class:" <+> pretty (sysClassClass s)
    , "Op Alt:" <+> pretty (sysOpAlt s) <+> "m"
    , "Timestamp:" <+> pretty (sysTimestamp s)
    , "Reserved:" <+> pretty (sysRsvd s)
    ]

data AuthMsg = AuthMsg{authType :: AuthType, pageNum :: Word8
  , lastPageIdx :: Word8, authLen :: Word8, authTimestamp :: Word32
  , authSig :: ByteString}
  deriving (Eq, Read, Show)

instance Binary AuthMsg where
  get = undefined
  put = undefined

instance Pretty AuthMsg where
  pretty = viaShow

data AuthType
  = AuthTyNone | UASIDSig | OpIDSig | MsgSetSig | AuthNRID | SpecificAuth
  | AuthRsvd Word8 | AuthPriv Word8
  deriving (Eq, Read, Show)

data AuthPage = AuthPage{authPageType :: AuthType, authPageNum :: Word8
  , authPageSig :: ByteString}
  deriving (Eq, Read, Show)

instance Binary AuthPage where
  get = getWord8 >>= \b ->
    let ty = undefined
        num = undefined
    in AuthPage ty num <$> getLazyByteString 23
  put = undefined

encodeAlt :: Double -> Word16
encodeAlt x = round $ (x + 1000) * 2

decodeAlt :: Word16 -> Double
decodeAlt x = fromIntegral x * 0.5 - 1000

encLatLon :: Double -> Int32
encLatLon = round . (* latLonMult)

decLatLon :: Int32 -> Double
decLatLon = (/ latLonMult) . fromIntegral

latLonMult :: Double
latLonMult = 10 ^ (7 :: Int)

encSpeed :: Double -> Double
encSpeed s | s <= 255 * 0.25 = s / 0.25
           | s > 255 * 0.25 && s < 254.25 = (s - (255 * 0.25)) / 0.75
           | otherwise = 254
