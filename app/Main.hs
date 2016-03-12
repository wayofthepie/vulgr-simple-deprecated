module Main where

import Data.Yaml
import System.Environment

import Lib
import Vulgr.Configuration (readConfFile)


main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> startOrFail filePath
        _          -> error "Please specify location of conf file."
  where
    startOrFail filePath = do
        eitherConf <- readConfFile filePath
        case eitherConf of
            Right conf   -> startApp conf
            Left pexcept -> error (prettyPrintParseException pexcept)
