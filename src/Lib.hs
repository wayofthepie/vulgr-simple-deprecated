{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    ) where

import Control.Monad.Trans.Either
import Data.Aeson
import Data.Aeson.TH
import Data.Text as T
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Debug.Trace

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)


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


$(deriveJSON defaultOptions ''User)

type API = "dependencies" :> Capture "appName" T.Text
                          :> Capture "version" T.Text
                          :> ReqBody '[JSON] GradleDependencySpec
                          :> Post '[JSON] T.Text

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = deps

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

deps :: T.Text -> T.Text-> GradleDependencySpec -> EitherT ServantErr IO T.Text
deps appName version gdeps= traceShow (show gdeps) $ return appName
