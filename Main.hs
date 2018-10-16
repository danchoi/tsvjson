module Main where
import Core
import TSVParser
import Data.Aeson
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Attoparsec.Text as AT
import Options.Applicative

{-

  This program takes TSV input stream and transforms the records
  into JSON objects, one per line, according to a field configuration
  list given as an argument or in a file.

-}

data Options = Options {
      _debugSpec :: Bool
    , _fieldSpecs :: [FieldSpec]
    }

pOptions :: Parser Options
pOptions = Options
    <$> switch (short 'd' <> help "Just debug spec and exit")
    <*> (
          concat <$> 
            many (argument readFieldSpec (metavar "[FIELDSPEC..]"))
        )


pOpts :: ParserInfo Options
pOpts = info (helper <*> pOptions)
             (header "tsvjson" <> fullDesc)

readFieldSpec :: ReadM [FieldSpec]
readFieldSpec = eitherReader $ \s -> 
    case AT.parseOnly pFieldSpecs (T.pack s) of
      Right x -> Right x
      Left err -> Left err


main :: IO ()
main = do
    Options{..} <- execParser pOpts
    if _debugSpec
    then
      mapM_ print _fieldSpecs
    else do
      xs <- (map (TL.splitOn "\t") . TL.lines) <$> TL.getContents
      let rs :: [Value]
          rs = map (toObject _fieldSpecs . map TL.toStrict) xs
      mapM_ (BL8.putStrLn . encode) rs


