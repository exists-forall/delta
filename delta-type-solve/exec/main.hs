module Main where

import ExternalProblem
import HandleExternalProblem (solveExternal)

import Data.Aeson (encode, eitherDecode')
import qualified Data.ByteString.Lazy as BL
import System.Exit (die)

main :: IO ()
main = do
  input <- BL.getContents
  problem <- case eitherDecode' input of
    Left err -> die $ "Could not parse JSON input: " ++ err
    Right problem -> return problem
  let solution = solveExternal problem
  BL.putStr $ encode solution
