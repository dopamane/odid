module Main (main) where

import Options.Applicative

main :: IO ()
main = do
  cli <- customExecParser prefs' pinfo
  case cli of
    ReadODID -> putStrLn "Reading"
    WriteODID -> putStrLn "Writing"

prefs' :: ParserPrefs
prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

pinfo :: ParserInfo CLI
pinfo = info (parser <**> helper) $ progDesc "Open Drone ID"

data CLI = ReadODID | WriteODID

parser :: Parser CLI
parser = hsubparser $ mconcat
  [ command "r" $ info (pure ReadODID) $ progDesc "Read ODID data"
  , command "w" $ info (pure WriteODID) $ progDesc "Write ODID data"
  ]
