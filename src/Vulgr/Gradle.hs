{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Vulgr.Gradle where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.HashMap.Strict as M
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Database.Neo4j as Neo
import qualified Database.Neo4j.Transactional.Cypher as TC

import Debug.Trace

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

data Project = Project
    { projName :: T.Text
    } deriving (Eq, Show)


-- | Analyzes the gradle dependency spec and builds a graph of this projects
-- dependencies and transitive dependencies in each configuration.
--
-- TODO : Abstract to a type class for operation with different build tools like
-- maven etc...
--
-- TODO : Why do we care about the results here...? If we do care, it should
-- relate to a Configuration (compile, archive, etc..).
graphGradleDeps :: GradleDependencySpec -> IO (Either TC.TransError [[TC.Result]])
graphGradleDeps gdeps = hardConn >>= \conn -> do
    result <- n4jTransaction conn $ do
        -- Currently a node name is the project name and version combined. These should really
        -- be two properties
        let proj = Project (gDepName gdeps <> ":" <> (textOrUndefined $ gDepVersion gdeps))

        TC.cypher ("MERGE ( n:PROJECT { name : {name}} )") (project2map proj)
        mapM (\config -> createAndRelate proj (traceShow (confName config) $ confName config)
            (confDeps config)) $ gDepConfigs gdeps
    return result


-- | Create the top-level direct dependency nodes and relate them to the root node.
createAndRelate :: Project -> T.Text -> Maybe [Dependency] -> TC.Transaction [TC.Result]
createAndRelate p relationName mdeps = case mdeps of
    Nothing   -> error "ERRRORORORORORO!!!!" -- FIXME: Remove this crap and be pure!
    Just deps ->
        -- FIXME : Need to sanitize relation names correctly!
        let rName = T.replace "-" "" relationName
        in  mapM (relateProjectAndDep p rName) $ deps


-- Relate a project and its direct dependency.
relateProjectAndDep :: Project -> T.Text -> Dependency -> TC.Transaction TC.Result
relateProjectAndDep p relationName d = do
    TC.cypher ("MERGE ( n:PROJECT { name : {name} } )") (dep2map d)
    res <- TC.cypher (
        "MATCH (a:PROJECT),(b:PROJECT)"
        <> "WHERE a.name = {pName}"
        <> "AND b.name = {dName}"
        <> "CREATE UNIQUE (a)-[r:" <> relationName <> "]->(b)"
        <> "RETURN r") $ M.fromList [
                (T.pack "pName", TC.newparam $ projName p)
                , (T.pack "dName", TC.newparam $ depName d)
                , (T.pack "relationName", TC.newparam $ relationName)
                ]
    res2 <- case depChildren d of
        Just deps -> graphTransitiveDeps (projName p) relationName d deps
        Nothing   -> pure ()

    return res


-- | Graph the projects transitive dependencies.
--
-- Currently this works as follows:
--  1. The list of dependencies are the dependencies of the parent depencency
--     which is a direct dfependency of the given project in the given configuration.
--  2. We recursively relate dependencies with their children with an edge corresponding
--     to the gradle configuration (compile, runtime etc...).
--
-- graphTransitiveDeps projectName configName parentDep deps
--
graphTransitiveDeps :: T.Text -> T.Text -> Dependency -> [Dependency] -> TC.Transaction ()
graphTransitiveDeps projectName configName parent deps =
    graphTransitiveDeps' projectName configName parent deps

graphTransitiveDeps' :: T.Text -> T.Text -> Dependency -> [Dependency] -> TC.Transaction ()
graphTransitiveDeps' _ _ _ [] = return ()
graphTransitiveDeps' pname cname parent (dep:deps) = do
    TC.cypher ("MERGE ( n:PROJECT { name : {name} } )") (dep2map dep)
    TC.cypher (
        "MATCH (a:PROJECT),(b:PROJECT)"
        <> "WHERE a.name = {parentName}"
        <> "AND b.name = {nextName}"
        <> "CREATE UNIQUE (a)-[r:" <> cname <> "]->(b)"
        <> "RETURN r") $ M.fromList [
                (T.pack "parentName", TC.newparam $ depName parent)
                , (T.pack "nextName", TC.newparam $ depName dep)
                , (T.pack "relationName", TC.newparam $ cname)
                ]

    -- If this dependency has children graph them.
    case depChildren dep of
        Just tdeps -> graphTransitiveDeps' pname cname dep tdeps
        Nothing    -> pure ()

    graphTransitiveDeps' pname cname parent deps


-- Helpers
project2map project =
    let pName = projName project
    in  M.fromList [
            (T.pack "name", TC.newparam pName)
            ]

dep2map dep = project2map $ Project (depName dep)

textOrUndefined :: Maybe T.Text -> T.Text
textOrUndefined maybeTxt = case maybeTxt of
    Just txt -> txt
    Nothing  -> "undefined"


n4jTransaction :: Neo.Connection -> TC.Transaction a ->  IO (Either TC.TransError a)
n4jTransaction conn action = flip Neo.runNeo4j conn $
    TC.runTransaction action

hardConn = (Neo.newAuthConnection (TE.encodeUtf8 "172.17.0.3") 7474 (TE.encodeUtf8 "neo4j", TE.encodeUtf8 "test"))
