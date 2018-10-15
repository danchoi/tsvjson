module Core where
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T

-- functionsn for turning [FieldSpec] and [Text] to [(Text, Value)]

data FieldSpec = FieldSpec {
    _type :: FType
  , _name :: Text
  } deriving Show           

data FType = FString
           | FNumber
           | FBool
           | FList FType [Text] -- list of separator strings e.g. [",", ";"]
    deriving Show           

mkConverter :: FieldSpec -> Text -> (Text, Value)
mkConverter (FieldSpec ftype name) v = (name, conv ftype v)

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
splitOnAll seps v = foldr f seps [v]
  where f sep acc = concatMap (T.splitOn sep) acc
          


