{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Vulgr.Dependency.Gradle (
    GradleDependencySpec
    , graph
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Hashable (Hashable, hash, hashWithSalt)
import qualified Data.HashMap.Strict as M
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Database.Neo4j as Neo
import qualified Database.Neo4j.Transactional.Cypher as TC

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


-- | The data to be stored in nodes
data NodeData = NodeData
    { name    :: T.Text
    , version :: Maybe T.Text
    , labels  :: [T.Text]
    } deriving (Eq, Show)

instance Hashable NodeData where
    hashWithSalt s (NodeData n v ls) = s + hash n + hash v + hash ls


-- | The data to be stored in relationships.
data RelationData = RelationData
    { config   :: T.Text
    , rootNode :: NodeData
    } deriving (Eq, Show)


instance Graphable GradleDependencySpec NodeData RelationData where
    type GNodeData GradleDependencySpec = NodeData
    type GRelationData GradleDependencySpec = RelationData
    graph gradleDeps =
       let rootNode  = NodeData (gDepName gradleDeps) (gDepVersion gradleDeps) []
           (_, initGraph) = consNode rootNode emptyUNGraph
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
    let thisNode  = NodeData (depName dep) Nothing []
        (thisNodeId, g') = consNode thisNode g
        g'' = consEdge parentNodeId thisNodeId (RelationData configName root) g'
    in case depChildren dep of
        Just children ->  case children of
                            [] -> parseDependencies g'' root thisNodeId deps configName
                            _  -> parseDependencies g'' root thisNodeId children configName
        Nothing ->  parseDependencies g'' root parentNodeId deps configName

