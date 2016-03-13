{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Vulgr.Graph.Persist where

import Control.Monad.Reader
import Data.Graph.Inductive (Gr, labNodes, prettify)
--import Data.Graph.Inductive.Basic
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Database.Neo4j as Neo
import qualified Database.Neo4j.Graph as NeoG
import qualified Database.Neo4j.Transactional.Cypher as TC
import qualified Database.Neo4j.Batch as B
import Database.Neo4j.Types

import Vulgr.Graph.Graphable

import Debug.Trace
persist :: UniqueNodeGraph NodeData RelationData -> Neo.Connection -> IO [T.Text]
persist g conn = do
    let gr = extract g
    c <- n4jBatch conn $ do
        mapM_ (\(_, n) -> createNode' n) (labNodes gr)
    let rels = NeoG.getRelationships c
    pure $ map (\r -> runRelPath $ relPath r) rels

createNode' (NodeData ps ls) =  B.createNode (M.fromList $ convert ps)

convert props = map (\(k, v) -> k |: v) props

instance  PropertyValueConstructor (Maybe T.Text) where
    newval maybeVal = case maybeVal of
                 Just val -> ValueProperty $ TextVal val
                 Nothing -> ValueProperty $ TextVal "Nothing"

n4jBatch :: Neo.Connection -> B.Batch a -> IO NeoG.Graph
n4jBatch conn action = flip Neo.runNeo4j conn $
    B.runBatch action


n4jTransaction :: Neo.Connection -> TC.Transaction a ->  IO (Either TC.TransError a)
n4jTransaction conn action = flip Neo.runNeo4j conn $
    TC.runTransaction action
