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
--import qualified Database.Neo4j as Neo
import qualified Database.Neo4j.Types as Neo
import qualified Database.Neo4j.Transactional.Cypher as TC
import Vulgr.Gradle

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree



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

class Graphable s where
    graph :: s -> Gr NodeData RelationData

type DependencyGraph = Gr NodeData RelationData
type GraphState = (H.HashMap NodeData Int, DependencyGraph)
--type GraphState = State (Int, DependencyGraph) DependencyGraph


instance Graphable GradleDependencySpec where
    graph gradleDeps =
       let rootNode  = NodeData (gDepName gradleDeps) (gDepVersion gradleDeps) []
           initGraphState = (H.empty, insNode (0,rootNode) empty)
       in  snd $ foldr (\config grs -> parseConfig grs config) initGraphState $ gDepConfigs gradleDeps


parseConfig :: GraphState -> Configuration -> GraphState
parseConfig grs (Configuration _ _ Nothing)   = grs
parseConfig grs (Configuration _ _ (Just [])) = grs
parseConfig grs (Configuration configName _ (Just deps)) = do
    parseDependencies grs 0 deps configName


parseDependencies :: GraphState -> Int -> [Dependency] -> T.Text -> GraphState
parseDependencies grs  _ [] _ = grs
parseDependencies grs parentNodeId (dep:deps) configName =
    let thisNode  = NodeData (depName dep) Nothing []
        (thisNodeId, (m, g)) = consNode thisNode grs
        g' = consEdge parentNodeId thisNodeId configName g
    in case depChildren dep of
        Just children ->  case children of
                            [] -> parseDependencies (m, g') thisNodeId deps configName
                            _  -> parseDependencies (m, g') thisNodeId children configName
        Nothing ->  parseDependencies (m, g') parentNodeId deps configName
  where
    getRootNode :: DependencyGraph -> NodeData
    getRootNode currGraph =
        case lab currGraph 0 of
            Just n -> n
            -- This is impossible because the root node, 0, always exists.
            Nothing -> NodeData "Impossible" Nothing []

    consEdge :: Int -> Int -> T.Text -> DependencyGraph -> DependencyGraph
    consEdge from to configName g =
        insEdge (from, to, RelationData configName (getRootNode g)) g

    -- | Construct a new node in the given dependency graph.
    consNode :: NodeData -> GraphState -> (Int, GraphState)
    consNode ndata (m, g) = create $ H.lookup ndata m
      where
        -- | Create a node in the graph if it does not already exist.
        create :: Maybe Int -> (Int, GraphState)
        create (Just nid) = (nid, (m, insNode (nid, ndata) g))
        create Nothing    = traceShow ("New node being created for " <> show ndata) $
            let [newNid] = newNodes 1 g
                m'       = H.insert ndata newNid m
            in  (newNid, (m', insNode (newNid, ndata) g))

