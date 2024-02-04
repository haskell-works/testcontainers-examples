
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Integration.TestContainers.Hasql where

import           Prelude

import qualified Hedgehog                                   as H
import qualified Hedgehog.Extras.Test                       as H
import           Test.Integration.TestContainers.Containers
import qualified Test.Tasty                                 as Tasty
import qualified Test.Tasty.Hedgehog                        as H
import qualified TestContainers.Tasty                       as TC

import           Data.Foldable                              (forM_)
import           Data.Functor.Contravariant                 (Contravariant (contramap),
                                                             (>$<))
import           Data.List.Split
import           Data.Text                                  (Text)
import qualified Data.Text                                  as Text
import qualified Data.Text.Encoding                         as Text
import qualified Hasql.Connection                           as Connection
import qualified Hasql.Decoders                             as Decoders
import qualified Hasql.Decoders                             as HD
import qualified Hasql.Encoders                             as Encoders
import qualified Hasql.Session                              as Session
import           Hasql.Statement                            (Statement (..))
import           Hedgehog                                   ((===))

{- HLINT ignore "Redundant flip" -}
{- HLINT ignore "Use camelCase" -}

data User = User
  { username :: Text
  , email    :: Text
  } deriving (Eq, Show)

runStatement :: String -> Statement () ()
runStatement sql = Statement (Text.encodeUtf8 $ Text.pack sql) encoder decoder True where
  encoder = contramap (const ()) Encoders.noParams
  decoder = HD.noResult

insertUser :: Statement User ()
insertUser =
  Statement sql encoder decoder True where
    sql = mconcat
      [ "INSERT INTO users ("
      , "username, email"
      , ") VALUES ("
      , "$1, $2"
      , ")"
      ]
    encoder =
      mconcat
        [ username >$< Encoders.param (Encoders.nonNullable Encoders.text)
        , email >$< Encoders.param (Encoders.nonNullable Encoders.text)
        ]
    decoder = HD.noResult

allUsers :: Statement () [User]
allUsers =
  Statement sql encoder decoder True where
    sql = mconcat
      [ "SELECT username, email "
      , "FROM users"
      ]
    encoder = contramap (const ()) Encoders.noParams
    decoder = HD.rowList row
    row =
      User
        <$> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)

tasty_postgres_integration_hasql_test :: Tasty.TestTree
tasty_postgres_integration_hasql_test =
  TC.withContainers setupContainers $ \start ->
    H.testProperty "Postgres.hasql" $ H.propertyOnce $ H.moduleWorkspace "tmp" $ \_tempDir -> do
      -- Actually start the containers!!
      PostgresEndpoint {postgresPort} <- H.evalIO start

      H.threadDelay 3000000

      let connectionSettings = Connection.settings "localhost" (fromIntegral postgresPort) "postgres" "12345" "postgres"
      H.noteShow_ connectionSettings
      Right connection <- H.evalIO $ Connection.acquire connectionSettings

      sql <- H.readFile "test/db/schema.sql"
      let statements = splitOn ";" sql

      result <- H.evalIO $ flip Session.run connection $ do
        forM_ statements $ \statement -> do
          Session.statement () $ runStatement statement
        flip Session.statement insertUser $ User
          { username = "markvshaney"
          , email = "markvshaney@gmail.com"
          }

        flip Session.statement allUsers ()

      rows <- H.leftFail result

      H.noteShow_ rows

      length rows === 1
