module Main where
import Test.HUnit
import Core


main :: IO Counts
main = runTestTT . test $ [

  "splitOnAll" ~:
      splitOnAll [",", ";"] "hello;world,quux" @?= ["hello","world","quux"]
  ]
