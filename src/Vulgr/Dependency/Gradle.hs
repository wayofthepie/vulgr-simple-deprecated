{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Vulgr.Dependency.Gradle (
    GradleDependencySpec
    , graph
    ) where

import Control.Parallel.Strategies
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Hashable (Hashable, hash, hashWithSalt)
import qualified Data.HashMap.Strict as M
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Vulgr.Graph.Graphable


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
    , confDesc :: Maybe T.Text
    , confDeps :: Maybe [Dependency]
--    , confModuleInsights :: Maybe [ModuleInsights] ignore for now
    } deriving (Eq, Show)

instance FromJSON Configuration where
    parseJSON (Object o) = Configuration
        <$> o .: "name"
        <*> o .:? "description"
        <*> o .:? "dependencies"

data Dependency = Dependency
    { depModule          :: Maybe T.Text
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



instance Graphable GradleDependencySpec where
    graph gradleDeps =
       let rootNode  = NodeData [("project", Just $ gDepName gradleDeps), ("version", gDepVersion gradleDeps)] []
           (_, initGraph) = consNode rootNode emptyGraph
           configs = gDepConfigs gradleDeps
       in  foldr (\config g -> parseConfig g rootNode config) initGraph configs


parseConfig :: UniqueNodeGraph NodeData RelationData
            -> NodeData
            -> Configuration
            -> UniqueNodeGraph NodeData RelationData
parseConfig g _ (Configuration _ _ Nothing)   = g
parseConfig g _ (Configuration _ _ (Just [])) = g
parseConfig g root (Configuration configName _ (Just deps)) = do
    parseDependencies g root 0 deps configName


parseDependencies :: UniqueNodeGraph NodeData RelationData
                  -> NodeData
                  -> Int
                  -> [Dependency]
                  -> T.Text
                  -> UniqueNodeGraph NodeData RelationData
parseDependencies g _  _ [] _ = g
parseDependencies g root parentNodeId (dep:deps) configName =
    let thisNode  = NodeData [("project", Just $ depName dep)] []
        (thisNodeId, g') = consNode thisNode g
        g'' = consEdge parentNodeId thisNodeId (RelationData [("config", Just configName), ("root", Just "fff")] []) g'
    in case depChildren dep of
        Just children ->  case children of
                            [] -> parseDependencies g'' root thisNodeId deps configName
                            _  -> parseDependencies g'' root thisNodeId children configName
        Nothing ->  parseDependencies g'' root parentNodeId deps configName

parFold f init = (foldr f init) . withStrategy (parList rseq)
