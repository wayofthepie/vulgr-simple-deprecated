{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Vulgr.Dependency.Npm where

import Data.Monoid
import Data.Aeson
import Data.Hashable (Hashable, hash, hashWithSalt)
import qualified Data.Map as M
import qualified Data.Text as T
import GHC.Generics

import Vulgr.Graph.Graphable

import Debug.Trace

data LongNpmDependencySpec = LongNpmDependencySpec
    { name :: T.Text
    , version :: Maybe T.Text
    , dependencies :: Maybe Dependencies
    , devDependencies :: Maybe DevDependencies
    } deriving (Eq, Generic, Show)

instance FromJSON LongNpmDependencySpec

type Dependencies = M.Map T.Text DependencyInfo
type DevDependencies = M.Map T.Text T.Text

{-
instance FromJSON Dependencies where
    parseJSON val = Dependencies <$> parseJSON val
-}

data DependencyInfo = DependencyInfo
    { diVersion :: Maybe T.Text
    , diDependencies :: Maybe Dependencies
    , diDevDependencies :: Maybe DevDependencies
--    , diOptionalDependencies :: Maybe Dependencies
    } deriving (Eq, Show)

instance FromJSON DependencyInfo where
    parseJSON (Object o) = DependencyInfo
        <$> o .:? "version"
        <*> o .:? "dependencies"
        <*> o .:? "devDependencies"
    --    <*> o .:? "optionalDependencies"
    parseJSON o = fail "Expecting an Object of type DependencyInfo!"

instance Graphable LongNpmDependencySpec where
    graph = graph'

graph' :: LongNpmDependencySpec -> UniqueNodeGraph NodeData RelationData
graph' longNpmSpec =
    parseDirect
        (dependencies longNpmSpec)
        (devDependencies longNpmSpec)
        (0, root)
        root $ initGraph root
  where
    initGraph n = snd $ consNode n emptyGraph
    root = NodeData [("name", Just $ name longNpmSpec), ("version", version longNpmSpec)] []

parseDirect :: Maybe Dependencies
            -> Maybe DevDependencies
            -> (Int, NodeData)
            -> NodeData
            -> UniqueNodeGraph NodeData RelationData
            -> UniqueNodeGraph NodeData RelationData
parseDirect deps devDeps n root g =
    parseDeps deps n root . parseDevDeps devDeps n root $ g

parseDeps :: Maybe Dependencies
          -> (Int, NodeData)
          -> NodeData
          -> UniqueNodeGraph NodeData RelationData
          -> UniqueNodeGraph NodeData RelationData
parseDeps maybeDeps n root g =
    maybe g id (maybeDeps >>= \deps ->
        pure $ parseDependencies n root "dependency" deps g)

parseDevDeps :: Maybe DevDependencies
             -> (Int, NodeData)
             -> NodeData
             -> UniqueNodeGraph NodeData RelationData
             -> UniqueNodeGraph NodeData RelationData
parseDevDeps maybeDevDeps (parentId, parentNode) root g =
    maybe g id (maybeDevDeps >>= \deps ->
        pure $ M.foldrWithKey (\k v g -> createAndLink parentId (k, v) g) g deps)
  where
    createAndLink :: Int
                  -> (T.Text, T.Text)
                  -> UniqueNodeGraph NodeData RelationData
                  -> UniqueNodeGraph NodeData RelationData
    createAndLink pid (k,v) g =
        let thisNode         = NodeData [("name", Just k), ("version", Just v)] []
            relationData     = RelationData [("root", Just "ddd"), ("type", Just "devDependency")] []
            (thisNodeId, g') = consNode thisNode g
            g''              = consEdge pid thisNodeId relationData g
        in  g''

parseDependencies :: (Int, NodeData)
                  -> NodeData
                  -> T.Text
                  -> Dependencies
                  -> UniqueNodeGraph NodeData RelationData
                  -> UniqueNodeGraph NodeData RelationData
parseDependencies (nodeId, node) root depType deps gr =
    M.foldrWithKey (\k v g -> parseDependency k v nodeId root depType g) gr deps


-- | parseDependency depName depInfo rootNode depType graph
parseDependency :: T.Text
                -> DependencyInfo
                -> Int
                -> NodeData
                -> T.Text
                -> UniqueNodeGraph NodeData RelationData
                -> UniqueNodeGraph NodeData RelationData
parseDependency depName depInfo parentNodeId root depType g =
    let thisNode = NodeData [("name", Just depName), ("version", diVersion depInfo)] []
        (thisNodeId,g') = consNode thisNode g
        g'' = consEdge parentNodeId thisNodeId (RelationData [("root", Just "fff"), ("type", Just depType)] []) g'
    in parseDirect (diDependencies depInfo) (diDevDependencies depInfo) (thisNodeId, thisNode) root g''




