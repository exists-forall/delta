module Main where

import Parse (parseModule)

import Data.Aeson (encode)
import qualified Data.Text.Lazy.IO as TextIO
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  input <- TextIO.getContents
  let parsed = parseModule input
  BL.putStr $ encode parsed
