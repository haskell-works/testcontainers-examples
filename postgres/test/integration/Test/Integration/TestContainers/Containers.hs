{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Integration.TestContainers.Containers
  ( PostgresEndpoint(..)
  , setupContainers
  ) where

import           Prelude

import           Data.Function
import qualified TestContainers.Tasty as TC

{- HLINT ignore "Use camelCase" -}

data PostgresEndpoint = PostgresEndpoint
  { postgresHost :: String
  , postgresPort :: Int
  }

-- | Sets up and runs the containers required for this test suite.
setupContainers :: TC.MonadDocker m => m PostgresEndpoint
setupContainers = do
  -- Launch the container based on the postgres image.
  mysqlContainer <- TC.run $ TC.containerRequest (TC.fromTag "postgres:16.1")
    -- Expose the port 5432 from within the container. The respective port
    -- on the host machine can be looked up using `containerPort` (see below).
    & TC.setExpose [ 5432 ]
    -- Wait until the container is ready to accept requests. `run` blocks until
    -- readiness can be established.
    & TC.setWaitingFor (TC.waitUntilMappedPortReachable 5432)
    & TC.setEnv [("POSTGRES_PASSWORD", "12345")]

  pure $ PostgresEndpoint
    { postgresHost = "0.0.0.0"
    , postgresPort =
        -- Look up the corresponding port on the host machine for the exposed
        -- port 5432.
        TC.containerPort mysqlContainer 5432
    }
