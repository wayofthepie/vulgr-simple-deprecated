{-# LANGUAGE DeriveGeneric #-}
module Vulgr.Configuration where

import qualified Data.Text as T
import Data.Yaml
import GHC.Generics
import System.FilePath ()


data Configuration = Configuration
    { n4jHost :: T.Text
    , n4jPort :: Int
    , n4jUser :: T.Text
    , n4jPass :: T.Text
    } deriving (Eq, Generic, Show)

instance FromJSON Configuration

readConfFile :: FilePath -> IO (Either ParseException Configuration)
readConfFile filep = decodeFileEither filep
