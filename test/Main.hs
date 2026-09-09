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
main = defaultMain $ testGroup "Test.ODID" [testMsgHdr, testBasicIDMsg]

testMsgHdr :: TestTree
testMsgHdr = testProperty "MsgHdr" $ property $ binTrip =<< forAll genMsgHdr

genMsgHdr :: MonadGen m => m MsgHdr
genMsgHdr = MsgHdr <$> Gen.word8 (Range.linear 0 15) <*> Gen.element msgTypes

testBasicIDMsg :: TestTree
testBasicIDMsg = testProperty "BasicIDMsg" $ property $ binTrip =<< forAll genBasicIDMsg

genBasicIDMsg :: MonadGen m => m BasicIDMsg
genBasicIDMsg =
  BasicIDMsg
    <$> Gen.enumBounded
    <*> Gen.enumBounded
    <*> BS.fromStrict `fmap` Gen.bytes (Range.singleton 20)

binTrip :: (MonadTest m, Show a, Eq a, Binary a) => a -> m ()
binTrip d = tripping d encode $ fmap (\(_, _, a) -> a) . decodeOrFail
