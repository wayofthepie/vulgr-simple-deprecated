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
import qualified Data.HashMap.Strict as M
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

data RelationData = RelationData
    { config   :: T.Text
    , rootNode :: NodeData
    } deriving (Eq, Show)

class Graphable s where
    graph :: s -> Gr NodeData RelationData

type DependencyGraph = Gr NodeData RelationData
type GraphState = (Int, DependencyGraph)
--type GraphState = State (Int, DependencyGraph) DependencyGraph


instance Graphable GradleDependencySpec where
    graph gradleDeps =
       let rootNode  = NodeData (gDepName gradleDeps) (gDepVersion gradleDeps) []
           initGraph = insNode (0,rootNode) empty
       in  snd $ foldr (\config grs -> parseConfig grs config) (0,initGraph) $ gDepConfigs gradleDeps


parseConfig :: GraphState -> Configuration -> GraphState
parseConfig grs (Configuration _ _ Nothing)   = grs
parseConfig grs (Configuration _ _ (Just [])) = grs
parseConfig grs (Configuration configName _ (Just deps)) = do
    parseDependencies grs 0 deps configName


parseDependencies :: GraphState -> Int -> [Dependency] -> T.Text -> GraphState
parseDependencies grs  _ [] _ = grs
parseDependencies grs parentIx (dep:deps) configName =
    let currIx    = fst grs
        currGraph = snd grs
        thisIx    = currIx + 1
        thisNode  = NodeData (depName dep) Nothing []
        g = consEdge parentIx thisIx configName $ consNode thisIx thisNode currGraph
    in case depChildren dep of
        Just children ->  case children of
                            [] -> parseDependencies (thisIx, g) thisIx deps configName
                            _  -> parseDependencies (thisIx, g) thisIx children configName
        Nothing ->  parseDependencies (thisIx, g) parentIx deps configName
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

    consNode :: Int -> NodeData -> DependencyGraph -> DependencyGraph
    consNode ix ndat = insNode (ix, ndat)

