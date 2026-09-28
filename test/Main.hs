{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Binary
import qualified Data.ByteString.Lazy as BS
import Data.ODID
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Test.ODID" [testBasicID, testLocation, testAuth
  , testSelfID, testSys, testOpID]

testBasicID :: TestTree
testBasicID = testCase "BasicID" $ do
  let input = Msg (MsgHdr 2 BasicIDTy) $
        BasicIDBdy CAARegID Heli "01234567890123456789" $ BS.replicate 3 0x00
  decode (encode input) @?= input

testLocation :: TestTree
testLocation = testCase "Location" $ do
  let input = Msg (MsgHdr 2 Location) $ LocBdy
        LocMsg{locOpStatus=Ground, locFlagsRsvd=False, locHeightType=AGL
          , locFlagsDir=False, locFlagsMult=False, locTrackDir=135
          , locSpeed=5, locVertSpeed=7.5, locLat=34.0522, locLon=118.2437
          , locPresAlt=10.5, locGeoAlt=8.5, locHeight=2
          , locVertAcc=VertAccLT3M, locHorzAcc=LT1M
          , locBaroAltAcc=VertAccLT3M, locSpeedAcc=LT1MS, locTimestamp=0
          , locTStampAccRsvd=0, locTStampAcc=0.1, locRsvd=0}
  decode (encode input) @?= input

testSelfID :: TestTree
testSelfID = testCase "SelfID" $ do
  let input = Msg (MsgHdr 2 SelfIDTy) $ SelfIDBdy 1 $ BS.pack [0..22]
  decode (encode input) @?= input

testOpID :: TestTree
testOpID = testCase "OperatorID" $ do
  let input = Msg (MsgHdr 2 OperatorID) $ OpIDBdy 0 (BS.pack [0..19]) $ BS.pack [1, 2, 3]
  decode (encode input) @?= input

testSys :: TestTree
testSys = testCase "System" $ do
  let input = Msg (MsgHdr 2 System) $ SysBdy
        SysMsg{sysClassType=EuroUnion, sysOpSrcType=Takeoff, sysOpLat=34.0575762
          , sysOpLon=(-118.2405026), sysArCnt=1, sysArRad=10, sysArCeil=10.5
          , sysArFloor=10.5, sysClassCat=Open, sysClassClass=3, sysOpAlt=24.5
          , sysTimestamp=12345, sysRsvd=0xAB}
  decode (encode input) @?= input

testAuth :: TestTree
testAuth = testCaseSteps "Auth" $ \step -> do
  step "Page0"
  let input = Msg (MsgHdr 2 Auth) $ AuthBdy $ AuthMsg AuthNone 0 (Just (0, 255, 120)) $
        BS.pack [0..16]
  decode (encode input) @?= input
  step "PageN"
  let input' = Msg (MsgHdr 2 Auth) $ AuthBdy $ AuthMsg UASIDSig 1 Nothing $ BS.pack [0..22]
  decode (encode input') @?= input'
