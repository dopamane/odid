{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Binary
import qualified Data.ByteString.Lazy as BS
import Data.ODID
import Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Test.ODID" [testBasicID]

testBasicID :: TestTree
testBasicID = testCase "BasicID" $ do
  let input = Msg (MsgHdr 2 BasicIDTy) $
        BasicIDBdy CAARegID Heli "01234567890123456789" $ BS.replicate 3 0x00
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
