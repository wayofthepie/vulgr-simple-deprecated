{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Vulgr.DependencySpec where

import Control.Parallel.Strategies
import Control.Monad
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

import Debug.Trace


type Neo4jT a = ReaderT Neo4jConfig (ResourceT IO) a

data Neo4jConfig = Neo4jConfig
    { conn :: Neo.Connection
    }

data Node = Node
    { name    :: T.Text
    , version :: T.Text
    } deriving (Eq, Show)

data RelationMetaData = RelationMetaData
    { config   :: T.Text
    , rootNode :: Node
    } deriving (Eq, Show)

graphGradleDeps2 :: GradleDependencySpec -> IO (Either TC.TransError ())
graphGradleDeps2 gdeps =
    let node = Node (gDepName gdeps) (mVerToText $ gDepVersion gdeps)
        configs = gDepConfigs gdeps
        --ts = map (configToGraph node) configs `using` parList rseq
    in hardConn >>= \conn -> do
        let action = createNode node >> mapM_ (configToGraph node) configs
        et <- n4jTransaction conn action
        case et of
            Right _ -> pure et
            Left (t1,t2) -> retryOnDeadlock action conn
  where
    configToGraph :: Node -> Configuration -> TC.Transaction ()
    configToGraph rootNode config =
        -- FIXME : Need to sanitize relation names correctly!
        let relationMetaData = RelationMetaData (T.replace "-" "" $ confName config) rootNode
        in case confDeps config of
            Nothing -> pure ()
            Just deps -> mapM_ (depToGraph rootNode relationMetaData) deps

    depToGraph :: Node -> RelationMetaData -> Dependency -> TC.Transaction ()
    depToGraph rootNode relationMetaData dep = do
        let thisNode = Node (depName dep) "test-version"

        createNode thisNode

        -- Relate this node to its direct reverse dependency.
        createAndRelateNeighbour rootNode relationMetaData thisNode

        -- Relate this node to its child dependencies, if they exist.
        case depChildren dep of
            Nothing -> pure ()
            Just children -> mapM_ (depToGraph thisNode relationMetaData) children

    mVerToText :: Maybe T.Text -> T.Text
    mVerToText maybeText = case maybeText of
                             Just txt -> txt
                             Nothing -> "undefined"

    retryOnDeadlock action conn =
        retrying
            (constantDelay 50000 <> limitRetries 5)
            (\rs b -> case b of
                    -- FIXME : Test for deadlock, we dont want to retry on all errors.
                    Left (errorName,_) -> pure True
                    Right _ -> pure False)
            (\rs -> n4jTransaction conn action)


createNode :: Node ->  TC.Transaction ()
createNode node =do
    TC.cypher ("MERGE ( n:PROJECT { name : {name}, version : {version}} )") $
        nodeToMap node
    pure ()

createAndRelateNeighbour :: Node -> RelationMetaData -> Node -> TC.Transaction TC.Result
createAndRelateNeighbour root relationMetaData neighbour = --traceShow ("C " ++ T.unpack (name root) ++ " " ++ T.unpack (name neighbour) )$
    createNode neighbour >> relateNodes root relationMetaData neighbour

-- | Relate two nodes.
-- FIXME : Currently assumes a from -> to relationship.
relateNodes :: Node -> RelationMetaData -> Node -> TC.Transaction TC.Result
relateNodes from relationMetaData to = findAndBuildRelation from relationMetaData to
  where
    -- Create the relationship between the two nodes. Merge here ensures
    -- we don't overwrite an existing relationship.
    findAndBuildRelation :: Node -> RelationMetaData -> Node -> TC.Transaction TC.Result
    findAndBuildRelation from relationMetaData to = do
        let root = rootNode relationMetaData
        let rootName = name root
        let rootVersion = version root
        let configName = config relationMetaData
        TC.cypher ("MATCH (a:PROJECT),(b:PROJECT)"
            <> "WHERE a.name = {fromName} and a.version = {fromVersion}"
            <> "AND b.name = {toName} and b.version = {toVersion}"
            <> "MERGE (a)-[r:" <> configName  <> "]->(b)"
            <> "ON CREATE SET r.rootName = {rootName}, r.rootVersion = {rootVersion}"
            <> "RETURN r") $ M.fromList [
                    (T.pack "fromName",     TC.newparam (name from))
                    , (T.pack "fromVersion",TC.newparam (version from))
                    , (T.pack "toName",     TC.newparam (name to))
                    , (T.pack "toVersion",  TC.newparam (version to))
                    , (T.pack "rootName",  TC.newparam rootName)
                    , (T.pack "rootVersion",  TC.newparam rootVersion)
                    ]

nodeToMap :: Node -> M.HashMap T.Text TC.ParamValue
nodeToMap (Node name version) =
    M.fromList [
        (T.pack "name", TC.newparam name)
        , (T.pack "version", TC.newparam version)
        ]
