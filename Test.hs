module Main where
import Test.HUnit
import Core
import Data.Text (Text)
import Data.Attoparsec.Text (parseOnly)
import Data.Aeson (toJSON, Value(..))


main :: IO Counts
main = runTestTT . test $ [
    "splitOnAll" ~:
      splitOnAll [",", ";"] "hello;world,quux" @?= ["hello","world","quux"]

  , "fieldspec 'title:string'" ~:
      parse' "title:string" @?= FieldSpec "title" FString

  , "fieldspec 'title'" ~:
      parse' "title" @?= FieldSpec "title" FString

  , "fieldspec 'num'" ~:
      parse' "rating:num" @?= FieldSpec "rating" FNumber

  , "fieldspec 'bool'" ~:
      parse' "rating:bool" @?= FieldSpec "rating" FBool

  , "fieldspec 'actors:[string]'" ~:
      parse' "actors:[string]" 
          @?= FieldSpec "actors" (FList FString [","])

  , "fieldspec 'actors:[string:,]'" ~:
      parse' "actors:[string:,]" 
          @?= FieldSpec "actors" (FList FString [","])

  , "fieldspec 'actors:[string:,;]'" ~:
      parse' "actors:[string:,;]" 
          @?= FieldSpec "actors" (FList FString [",",";"])

  , "fieldspec 'ratings:[num:,]'" ~:
      parse' "ratings:[num:,]" 
          @?= FieldSpec "ratings" (FList FNumber [","])

  , "fieldspec 'ratings:[number:,]'" ~:
      parse' "ratings:[number:,]" 
          @?= FieldSpec "ratings" (FList FNumber [","])


  , "conv list" ~:
      conv (FList FString [","]) "red,green"
        @?= toJSON ["red", "green"]

  , "conv null" ~:
      conv FString "" @?= Null
  ]



parse' :: Text -> FieldSpec
parse' = either error id . parseOnly pFieldSpec
