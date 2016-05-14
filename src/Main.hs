module Main where

import qualified IndexSet
import qualified PosetSubsetSession
import PosetSubsetSession (buildPoset, Relation(ChildOf))
import qualified TopoSort
import qualified DirectedGraph
import qualified Families

import qualified Text.Show.Pretty as Pretty
import Data.Maybe (fromJust)

p :: (Show a) => a -> IO ()
p = putStrLn . Pretty.ppShow

html :: (Show a) => a -> String
html = fromJust . fmap (Pretty.valToHtmlPage Pretty.defaultHtmlOpts) . Pretty.parseValue . show

main :: IO ()
main = p $ buildPoset
  [ "UInt" `ChildOf` "Int"
  , "Int" `ChildOf` "Fraction"
  , "Fraction" `ChildOf` "Float"
  , "Int" `ChildOf` "Float"
  , "Float" `ChildOf` "Complex"
  , "UnitComplex" `ChildOf` "Complex"

  , "Node" `ChildOf` "Seq"
  , "End" `ChildOf` "Seq"
  ]
