{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Binary
import qualified Data.ByteString.Lazy as BS
import Data.ODID
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Test.ODID" [loopback "BasicID" basicID
  , loopback "Location" loc, loopback "Auth0" auth0, loopback "AuthN" authN
  , loopback "SelfID" selfID, loopback "Sys" sys, loopback "OpID" opID
  , loopback "Pack" msgPack]

loopback :: (Binary a, Eq a, Show a) => String -> a -> TestTree
loopback n d = testCase n $ decode (encode d) @?= d

basicID :: Msg
basicID = Msg (MsgHdr 2 BasicIDTy) $
  BasicIDBdy CAARegID Heli "01234567890123456789" $ BS.replicate 3 0x00

loc :: Msg
loc = Msg (MsgHdr 2 Location) $ LocBdy
  LocMsg{locOpStatus=Ground, locFlagsRsvd=False, locHeightType=AGL
        , locFlagsDir=False, locFlagsMult=False, locTrackDir=135
        , locSpeed=5, locVertSpeed=7.5, locLat=34.0522, locLon=118.2437
        , locPresAlt=10.5, locGeoAlt=8.5, locHeight=2
        , locVertAcc=VertAccLT3M, locHorzAcc=LT1M
        , locBaroAltAcc=VertAccLT3M, locSpeedAcc=LT1MS, locTimestamp=0
        , locTStampAccRsvd=0, locTStampAcc=0.1, locRsvd=0}

selfID :: Msg
selfID = Msg (MsgHdr 2 SelfIDTy) $ SelfIDBdy 1 $ BS.pack [0..22]

opID :: Msg
opID = Msg (MsgHdr 2 OperatorID) $ OpIDBdy 0 (BS.pack [0..19]) $ BS.pack [1, 2, 3]

sys :: Msg
sys = Msg (MsgHdr 2 System) $ SysBdy
  SysMsg{sysClassType=EuroUnion, sysOpSrcType=Takeoff, sysOpLat=34.0575762
        , sysOpLon=(-118.2405026), sysArCnt=1, sysArRad=10, sysArCeil=10.5
        , sysArFloor=10.5, sysClassCat=Open, sysClassClass=3, sysOpAlt=24.5
        , sysTimestamp=12345, sysRsvd=0xAB}

auth0 :: Msg
auth0 = Msg (MsgHdr 2 Auth) $ AuthBdy $ AuthMsg AuthNone 0 (Just (0, 255, 120)) $
  BS.pack [0..16]

authN :: Msg
authN = Msg (MsgHdr 2 Auth) $ AuthBdy $ AuthMsg UASIDSig 1 Nothing $ BS.pack [0..22]

msgPack :: Msg
msgPack = Msg (MsgHdr 2 Pack) $ PackBdy 0x19 7 [basicID, loc, selfID, opID, sys, auth0, authN]
