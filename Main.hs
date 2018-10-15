module Main where
import Core
import TSVParser
import Data.Aeson
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL8
import Options.Applicative

{-

  This program takes TSV input stream and transforms the records
  into JSON objects, one per line, according to a field configuration
  list given as an argument or in a file.

-}


main :: IO ()
main = do
  bs <- BL8.getContents
  putStrLn "hello world"
