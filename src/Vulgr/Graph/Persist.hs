{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Vulgr.Graph.Persist where

import Control.Monad.Reader
import Data.Graph.Inductive (Gr, labNodes, prettify)
--import Data.Graph.Inductive.Basic
import qualified Data.HashMap.Strict as M
import Data.Pool
import qualified Data.Text as T
import qualified Database.Neo4j as Neo
import qualified Database.Neo4j.Graph as NeoG
import qualified Database.Neo4j.Transactional.Cypher as TC
import qualified Database.Neo4j.Batch as B
import Database.Neo4j.Types

import Vulgr.Graph.Graphable

import Debug.Trace

persist :: UniqueNodeGraph NodeData RelationData -> Pool Neo.Connection -> IO (Either TC.TransError ())
persist g pool = withResource pool $ \conn -> do
    let gr = extract g
    let dat = foldr (\n m -> createNodeData m n) (M.empty, []) (labNodes gr)
    n4jTransaction conn $ do
        mapM_ (\c -> createNode c) (snd dat)
    n4jTransaction conn $ do
        mapM_ (\c -> createRelation c)



createNodeData :: (M.HashMap Int NodeData, [(T.Text, M.HashMap T.Text TC.ParamValue)])
               -> (Int, NodeData)
               -> (M.HashMap Int NodeData, [(T.Text, M.HashMap T.Text TC.ParamValue)])
createNodeData (m, l) (nid, n) =
    let c = genCypherCreateNode n
        m' = M.insert nid n m
    in  (m', c:l)

createNode (query, m) = TC.cypher query m




instance  PropertyValueConstructor (Maybe T.Text) where
    newval maybeVal = case maybeVal of
                 Just val -> ValueProperty $ TextVal val
                 Nothing -> ValueProperty $ TextVal "Nothing"

-- | Generates a MERGE cypher call for the given node.
--
-- FIXME : Currently this is not type safe, or sanitized!
genCypherCreateNode :: NodeData -> (T.Text, M.HashMap T.Text TC.ParamValue)
genCypherCreateNode (NodeData props labels) =
    ("MERGE ( n" <> labelsStr <> "{ b :\"test\"" <> propsStr <> " }" <> " )", propsMap)
  where
    labelsStr = foldr (\l t -> t <> ":" <> l) T.empty labels
    propsStrBuilder  = foldr (\(k, v) (t, m) ->
        (t <> "," <> k <> ":" <> " {" <> k <> "} ", M.insert k (TC.newparam v) m))
        (T.empty, M.empty)
        props
    propsStr = fst propsStrBuilder
    propsMap = snd propsStrBuilder

--genCypherCreateRelation :: M.HashMap Nodedata Int -> RelationData -> T.Text

n4jBatch :: Neo.Connection -> B.Batch a -> IO NeoG.Graph
n4jBatch conn action = flip Neo.runNeo4j conn $
    B.runBatch action


n4jTransaction :: Neo.Connection -> TC.Transaction a ->  IO (Either TC.TransError a)
n4jTransaction conn action = flip Neo.runNeo4j conn $
    TC.runTransaction action
