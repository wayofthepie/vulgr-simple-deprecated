{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where

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

import Debug.Trace

data GradleDependencySpec = GradleDependencySpec
    { gDepName     :: T.Text
    , gDepDesc     :: Maybe T.Text
    , gDepConfigs  :: [Configuration]
    , gDepVersion  :: Maybe T.Text -- FIXME: SemVer?
    } deriving (Eq, Show)

instance FromJSON GradleDependencySpec where
    parseJSON (Object o) = GradleDependencySpec
        <$> o .: "name"
        <*> o .:? "description"
        <*> o .: "configurations"
        <*> o .:? "version"


data Configuration = Configuration
    { confName :: T.Text
    , confDesc :: T.Text
    , confDeps :: Maybe [Dependency]
--    , confModuleInsights :: Maybe [ModuleInsights] ignore for now
    } deriving (Eq, Show)

instance FromJSON Configuration where
    parseJSON (Object o) = Configuration
        <$> o .: "name"
        <*> o .: "description"
        <*> o .: "dependencies"

data Dependency = Dependency
    { depModule          :: T.Text
    , depName            :: T.Text
    , depResolvable      :: Bool
    , depHasConflict     :: Maybe Bool
    , depAlreadyRendered :: Bool
    , depChildren        :: Maybe [Dependency]
    } deriving (Eq, Show)

instance FromJSON Dependency where
    parseJSON (Object o) = Dependency
        <$> o .: "module"
        <*> o .: "name"
        <*> o .: "resolvable"
        <*> o .:? "hasConfict"
        <*> o .: "alreadyRendered"
        <*> o .: "children"

data Product = Product
    { prodName :: T.Text
    , prodVersion :: T.Text
    } deriving (Eq, Show)

type API =
    "graph" :> Capture "appName" T.Text
                   :> Capture "version" T.Text
                   :> ReqBody '[JSON] GradleDependencySpec
                   :> Post '[JSON] T.Text
--    :<|> "graph" :>

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = deps

deps :: T.Text -> T.Text-> GradleDependencySpec -> EitherT ServantErr IO T.Text
deps appName version gdeps = do
    eitherResult <- liftIO $ hardConn >>= flip n4jTransaction (uniqProductNodeCypher $ Product appName version)
    case eitherResult of
        Right _ -> pure ("Created dependency graph for " <> appName <> " " <> version)
        Left _  -> pure "Error...!"


uniqProductNodeCypher :: Product -> TC.Transaction TC.Result
uniqProductNodeCypher product =
    TC.cypher ("MERGE ( n:PRODUCT { name : {name}, version : {version} } )") (product2map product)
  where
    product2map product = M.fromList [
        (T.pack "name", TC.newparam (prodName product))
        , (T.pack "version", TC.newparam (prodVersion product))
        ]

n4jTransaction :: Neo.Connection -> TC.Transaction a -> IO (Either TC.TransError a)
n4jTransaction conn action = flip Neo.runNeo4j conn $
    TC.runTransaction action

hardConn = (Neo.newAuthConnection (TE.encodeUtf8 "172.17.0.3") 7474 (TE.encodeUtf8 "neo4j", TE.encodeUtf8 "test"))
