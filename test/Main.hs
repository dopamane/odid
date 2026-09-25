{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Binary
import qualified Data.ByteString.Lazy as BS
import Data.ODID
--import Hedgehog
--import qualified Hedgehog.Gen   as Gen
--import qualified Hedgehog.Range as Range
import Test.Tasty
--import Test.Tasty.Hedgehog
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Test.ODID" [testBasicID, testLocation, testSelfID, testOpID]

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

{-
testMsgBinaryTrip :: TestTree
testMsgBinaryTrip = testProperty "Msg" $ property $ binTrip =<< forAll genMsg

genMsg :: MonadGen m => m Msg
genMsg = do
  ver <- Gen.word8 $ Range.linear 0 15
  typ <- Gen.element msgTypes
  Msg (MsgHdr ver typ) <$> case typ of
    BasicIDTy -> BasicIDBdy <$> Gen.enumBounded <*> Gen.enumBounded
      <*> BS.fromStrict `fmap` Gen.bytes (Range.singleton 20)
      <*> BS.fromStrict `fmap` Gen.bytes (Range.singleton 3)
    Location -> LocBdy <$> Gen.discard
    Auth -> Gen.discard
    SelfIDTy -> SelfIDBdy <$> Gen.enumBounded
      <*> BS.fromStrict `fmap` Gen.bytes (Range.singleton 23)
    System -> Gen.discard
    OperatorID -> OpIDBdy <$> Gen.enumBounded
      <*> BS.fromStrict `fmap` Gen.bytes (Range.singleton 20)
      <*> BS.fromStrict `fmap` Gen.bytes (Range.singleton 3)
    Pack -> Gen.discard

genOpStatus :: MonadGen m => m OpStatus
genOpStatus = Gen.choice $ OpStatusRsvd `fmap` Gen.word8 (Range.linear 0 15) : map pure
  [Undeclared, Ground, Airborne, Emergency, RemoteIDSystemFailure]

binTrip :: (MonadTest m, Show a, Eq a, Binary a) => a -> m ()
binTrip d = tripping d encode $ fmap (\(_, _, a) -> a) . decodeOrFail
-}
