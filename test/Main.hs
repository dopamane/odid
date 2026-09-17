module Main (main) where

import Data.Binary
import qualified Data.ByteString.Lazy as BS
import Data.ODID
import Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

main :: IO ()
main = defaultMain $ testGroup "Test.ODID" [testMsgBinaryTrip]

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
    Location -> Gen.discard
    Auth -> Gen.discard
    SelfIDTy -> SelfIDBdy <$> Gen.enumBounded
      <*> BS.fromStrict `fmap` Gen.bytes (Range.singleton 23)
    System -> Gen.discard
    OperatorID -> OpIDBdy <$> Gen.enumBounded
      <*> BS.fromStrict `fmap` Gen.bytes (Range.singleton 20)
      <*> BS.fromStrict `fmap` Gen.bytes (Range.singleton 3)
    Pack -> Gen.discard

binTrip :: (MonadTest m, Show a, Eq a, Binary a) => a -> m ()
binTrip d = tripping d encode $ fmap (\(_, _, a) -> a) . decodeOrFail
