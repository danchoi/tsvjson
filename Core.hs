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


-- top level function

toObject :: [FieldSpec] -> [Text] -> Value
toObject ffs vs = object [ k .= v | (k,v) <- toPairs ffs vs ]

toPairs :: [FieldSpec] -> [Text] -> [(Text, Value)]
toPairs ffs vs =
    let converters = map mkConverter ffs
    in zipWith ($) converters vs


mkConverter :: FieldSpec -> Text -> (Text, Value)
mkConverter (FieldSpec name ftype) v = (name, conv ftype v)

-- null or blank string always becomes Null
conv :: FType -> Text -> Value
conv _ "" = Null
conv _ "null" = Null
conv _ "-" = Null
conv _ "\\N" = Null
conv FString v = String v
conv FNumber v = Number (read . T.unpack $ v)
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
    number (long form of num ok)
    bool
    [type[:seps]] -- list type with separator

If [:type] is omitted, it is inferred to be string.

List types:

    [string]
    red,green,blue -> ["red", "green", "blue"]

    [string:,;]
    red,green;blue -> ["red", "green", "blue"]

    [num]
    1,2,3 -> [1,2,3]

    [string:,]
    "" -> []
-}

pFieldSpecs :: Parser [FieldSpec]
pFieldSpecs = AT.sepBy1 pFieldSpec whiteSpace

-- | at least one whitespace character "\t\n "
whiteSpace :: Parser ()
whiteSpace = 
    AT.takeWhile1 (AT.inClass "\t\n ") >> return ()

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
pFieldName = takeWhile1 (notInClass ": \t\n")

pFieldType :: Parser FType
pFieldType = choice [
    string "string" *> pure FString
  , string "num" *>  optional (string "ber") *> pure FNumber
  , string "bool" *> pure FBool
  , pListType
  ]

pListType :: Parser FType
pListType = 
  char '[' *>
  (
  FList 
      <$> pFieldType
      <*> (

            (char ':' *> 
              (mkSeps <$> (takeWhile1 (notInClass " ]")))
            )
            <|> 
            pure [","]
          )
  )
  <* char ']'

mkSeps :: Text -> [Text]
mkSeps = map T.singleton . T.unpack

