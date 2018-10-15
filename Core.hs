module Core where
import Control.Applicative
import Data.Aeson hiding (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text 
import qualified Data.Attoparsec.Text as AT

-- functions for turning [FieldSpec] and [Text] to [(Text, Value)]

data FieldSpec = FieldSpec {
    _name :: Text
  , _type :: FType
  } deriving (Eq, Show)

data FType = FString
           | FNumber
           | FBool
           | FList FType [Text] -- list of separator strings e.g. [",", ";"]
    deriving (Eq, Show)

mkConverter :: FieldSpec -> Text -> (Text, Value)
mkConverter (FieldSpec name ftype) v = (name, conv ftype v)

-- null or blank string always becomes Null
conv :: FType -> Text -> Value
conv _ "" = Null
conv _ "null" = Null
conv _ "-" = Null
conv _ "\\N" = Null
conv FString v = String v
conv FNumber v = Number (read . show . T.unpack $ v)
conv FBool v = Bool $ case v of
                "true" -> True
                "t" -> True
                "TRUE" -> True
                "YES" -> True
                "yes" -> True
                "y" -> True
                "1" -> True
                _ -> False
conv (FList ftype seps) v = toJSON $ map (conv ftype) $ splitOnAll seps v

splitOnAll :: [Text] -> Text -> [Text]
splitOnAll seps v = foldr f [v] seps
  where f sep acc = concatMap (T.splitOn sep) acc
          
------------------------------------------------------------------------
-- FieldSpec DSL parser

{- FieldSpec as given on the command line

    name[:type]

where type is 

    string
    num
    bool
    list[:type[:seps]] -- defaults to string with comma sep

If type is omitted, it is string.

    list:string:, 
    red,green,blue -> ["red", "green", "blue"]

    list:string:[,;]
    red,green;blue -> ["red", "green", "blue"]

    list:number:, 
    1,2,3 -> [1,2,3]

    list:string:, 
    "" -> []
-}

pFieldSpec :: Parser FieldSpec
pFieldSpec = 
    FieldSpec 
      <$> pFieldName 
      <*> (
            (char ':' *> pFieldType)
            <|>
            (pure FString)
          )

pFieldName :: Parser Text
pFieldName = takeWhile1 (notInClass ": ")

pFieldType :: Parser FType
pFieldType = choice [
    string "string" *> pure FString
  , string "num" *> pure FNumber
  , string "bool" *> pure FBool
  , string "list" *> pListType
  ]

pListType :: Parser FType
pListType = 
  FList 
      <$> ( 
            (char ':' *> pFieldType)
            <|> 
            pure FString
          )
      <*> (

            (char ':' *> 
              (mkSeps <$> (takeWhile1 (notInClass " ")))
            )
            <|> 
            pure [","]
          )

mkSeps :: Text -> [Text]
mkSeps = map T.singleton . T.unpack


