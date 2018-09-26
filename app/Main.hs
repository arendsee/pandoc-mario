module Main where

-- this example is adapted from an example in pandoc docs

import Text.Pandoc.JSON
import PandocMario

main :: IO ()
main = simpleSearch "foo berry pickles"
