-- | Open Drone ID
module Data.ODID (ODID(..), readODID, writeODID) where

import Data.ByteString.Lazy (ByteString)

data ODID = ODID

readODID :: ByteString -> Either String ODID
readODID = undefined

writeODID :: ODID -> ByteString
writeODID = undefined
