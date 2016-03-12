{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Monoid ((<>))
import qualified Data.Aeson.TH
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as M
import qualified Database.Neo4j as Neo
import qualified Database.Neo4j.Transactional.Cypher as TC
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Vulgr.Graph.Graphable (getGraph, pretty)
import Vulgr.Dependency.Gradle

import Debug.Trace

type API =
    "graph" :> ReqBody '[JSON] GradleDependencySpec
            :> Post '[JSON] T.Text
    :<|> "hello" :> Get '[JSON] T.Text
{-    :<|> "graph"
            :> Capture "appName" T.Text
            :> Capture "version" T.Text
            :> Get '[JSON] T.Text
-}

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = graphDeps :<|> hello

graphDeps :: GradleDependencySpec -> EitherT ServantErr IO T.Text
graphDeps gdeps = do
    eitherResult <-  liftIO (pure . Right . pretty $ graph gdeps)
    case eitherResult of
        Right r -> pure r
        Left err  -> pure $ (fst err) <> " : " <> (snd err)

hello :: EitherT ServantErr IO T.Text
hello = pure "hello!!"
