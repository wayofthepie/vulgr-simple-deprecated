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
import Vulgr.Graph.Graphable (getGraph, pretty)

import Debug.Trace

type API =
    "graph" :> ReqBody '[JSON] GradleDependencySpec
            :> Post '[JSON] T.Text
    :<|> "hello" :> Get '[JSON] T.Text


newtype App a = App { runApp :: ReaderT Neo.Connection IO a }
    deriving (Monad, Functor, Applicative, MonadReader Neo.Connection, MonadIO)

startApp :: Configuration -> IO ()
startApp (Configuration host port user pass) =
    let hostBs = TE.encodeUtf8 host
        userBs = TE.encodeUtf8 user
        passBs = TE.encodeUtf8 pass
    in do
        conn <- Neo.newAuthConnection hostBs port (userBs, passBs)
        run 8080 (app conn)

app :: Neo.Connection -> Application
app conn = serve api (readerServer conn)

api :: Proxy API
api = Proxy

readerServer :: Neo.Connection -> Server API
readerServer conn = enter (Nat (runAppT conn)) readerServerT

readerServerT :: ServerT API App
readerServerT = graphDeps :<|> hello

runAppT :: Neo.Connection -> App a -> EitherT ServantErr IO a
runAppT conn action = liftIO (runReaderT (runApp action) conn)


graphDeps :: GradleDependencySpec -> App T.Text
graphDeps gdeps = do
    eitherResult <-  liftIO (pure . Right . pretty $ graph gdeps)
    case eitherResult of
        Right r -> traceShow ("req") $ pure r
        Left err-> pure $ (fst err) <> " : " <> (snd err)

hello :: App T.Text
hello = pure "hello!!"
