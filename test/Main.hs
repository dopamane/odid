module Main (main) where

import Data.Binary
import Data.ODID
import Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

main :: IO ()
main = defaultMain $ testGroup "Test.ODID" [testMsgHdr]

testMsgHdr :: TestTree
testMsgHdr = testProperty "MsgHdr" $ property $ do
  h <- forAll $ MsgHdr <$> Gen.word8 (Range.linear 0 15) <*> Gen.element msgTypes
  tripping h encode $ fmap (\(_, _, a) -> a) . decodeOrFail
