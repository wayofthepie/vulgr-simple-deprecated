{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib where

import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Monoid ((<>))
import Data.Pool
import Data.Time.Clock
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as M
import qualified Database.Neo4j as Neo
import qualified Database.Neo4j.Transactional.Cypher as TC
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Vulgr.Configuration (Configuration(..))
import Vulgr.Dependency.Gradle
import Vulgr.Dependency.Npm
import Vulgr.Graph.Graphable (pretty)
--import Vulgr.Graph.Persist (persist)

import Debug.Trace

type API =
    "graph" :> ReqBody '[JSON] LongNpmDependencySpec
            :> Post '[JSON] T.Text
    :<|> "hello" :> Get '[JSON] T.Text


newtype App a = App { runApp :: ReaderT (Pool Neo.Connection) IO a }
    deriving (Monad, Functor, Applicative, MonadReader (Pool Neo.Connection), MonadIO)

startApp :: Configuration -> IO ()
startApp (Configuration host port user pass) =
    let hostBs = TE.encodeUtf8 host
        userBs = TE.encodeUtf8 user
        passBs = TE.encodeUtf8 pass
    in do
        pool  <- createPool (Neo.newAuthConnection hostBs port (userBs, passBs))
                            (\c -> putStrLn "Destroying... Haha not!" ) -- fixme, destroy connections!
                            6
                            10
                            10
        run 8080 (app pool)

app :: Pool Neo.Connection -> Application
app conn = serve api (readerServer conn)

api :: Proxy API
api = Proxy

readerServer :: Pool Neo.Connection -> Server API
readerServer pool = enter (Nat (runAppT pool)) readerServerT

readerServerT :: ServerT API App
readerServerT = graphDeps :<|> hello

runAppT :: Pool Neo.Connection -> App a -> EitherT ServantErr IO a
runAppT pool action = liftIO (runReaderT (runApp action) pool)


graphDeps :: LongNpmDependencySpec -> App T.Text
graphDeps gdeps = do
    let g = graph gdeps
    eitherResult <-  (pure . Right . pretty $ g)
    case eitherResult of
        Right _ -> pure "Created."
        Left err-> pure $ (fst err) <> " : " <> (snd err)

hello :: App T.Text
hello = pure "hello!!"
