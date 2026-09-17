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

genMsgHdr :: MonadGen m => m MsgHdr
genMsgHdr = MsgHdr <$> Gen.word8 (Range.linear 0 15) <*> Gen.element msgTypes

genMsg :: MonadGen m => m Msg
genMsg = Msg <$> genMsgHdr <*> genBasicIDBdy

genBasicIDBdy :: MonadGen m => m MsgBdy
genBasicIDBdy = BasicIDBdy <$> Gen.enumBounded <*> Gen.enumBounded
  <*> BS.fromStrict `fmap` Gen.bytes (Range.singleton 20)
  <*> BS.fromStrict `fmap` Gen.bytes (Range.singleton 3)

binTrip :: (MonadTest m, Show a, Eq a, Binary a) => a -> m ()
binTrip d = tripping d encode $ fmap (\(_, _, a) -> a) . decodeOrFail
