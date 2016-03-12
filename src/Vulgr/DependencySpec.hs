{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Vulgr.DependencySpec where

import Control.Parallel.Strategies
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Control.Retry
import Data.Functor
import Data.Hashable
import qualified Data.HashMap.Strict as H
import Data.Monoid
import qualified Data.Text as T
import Data.Maybe (maybe)
--import qualified Database.Neo4j as Neo
import qualified Database.Neo4j.Types as Neo
import qualified Database.Neo4j.Transactional.Cypher as TC
import Vulgr.Gradle

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

import Vulgr.Graph.Graphable

import Debug.Trace


type Neo4jT a = ReaderT Neo4jConfig (ResourceT IO) a

data Neo4jConfig = Neo4jConfig
    { conn :: Neo.Connection
    }

data NodeData = NodeData
    { name    :: T.Text
    , version :: Maybe T.Text
    , labels  :: [T.Text]
    } deriving (Eq, Show)

instance Hashable NodeData where
    hashWithSalt s (NodeData n v ls) = s + hash n + hash v + hash ls

data RelationData = RelationData
    { config   :: T.Text
    , rootNode :: NodeData
    } deriving (Eq, Show)


type DependencyGraph = Gr NodeData RelationData

--instance NodeConstraints NodeData

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

