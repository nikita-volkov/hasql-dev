-- |
-- Capabilities-based algebra for integrating various abstractions of Hasql with the purpose of eradicating boilerplate code and making the experience smooth.
--
-- == Capabilities
--
-- By capabilities we mean classes empowering a context with functionality.
--
-- E.g., with the help of the 'RunsStatement' typeclass 'Statement.Statement' can be executed in 'Session', 'Pipeline' and 'Transaction'.
--
-- Besides letting us connect different abstractions of Hasql together it also allows to integrate these functions directly into custom execution contexts like the main application-specific monad.
--
-- == Statement-based Modularisation
--
-- This library also provides a typeclass 'IsStatementParams' supporting a modularisation pattern, where you define everything related to one statement in an isolated module.
-- This pattern leads to high code cohesion and low coupling.
--
-- === __Example of such a module__
--
-- > module MusicCatalogueDb.Statements.SelectArtistIdsByName where
-- >
-- > import Data.Functor.Contravariant
-- > import Data.Text (Text)
-- > import Data.UUID (UUID)
-- > import Data.Vector (Vector)
-- > import qualified Hasql.Decoders as Decoders
-- > import qualified Hasql.Encoders as Encoders
-- > import HasqlDev
-- > import Prelude
-- >
-- > data SelectArtistIdsByNameParams = SelectArtistIdsByNameParams
-- >   { name :: Text
-- >   }
-- >
-- > type SelectArtistIdsByNameResult = Vector SelectArtistIdsByNameResultRow
-- >
-- > data SelectArtistIdsByNameResultRow = SelectArtistIdsByNameResultRow
-- >   { id :: UUID
-- >   }
-- >
-- > instance IsStatementParams SelectArtistIdsByNameParams where
-- >   type StatementResultByParams SelectArtistIdsByNameParams = SelectArtistIdsByNameResult
-- >   statementByParams =
-- >     Statement sql encoder decoder canBePrepared
-- >     where
-- >       sql = "select id from artist where name = $1 limit 1"
-- >       encoder =
-- >         mconcat
-- >           [ (\(SelectArtistIdsByNameParams x) -> x)
-- >               >$< Encoders.param (Encoders.nonNullable Encoders.text)
-- >           ]
-- >       decoder =
-- >         Decoders.rowVector
-- >           ( SelectArtistIdsByNameResultRow
-- >               <$> Decoders.column (Decoders.nonNullable Decoders.uuid)
-- >           )
-- >       canBePrepared = True
module HasqlDev
  ( -- * Connection Pool
    Pool.Pool,
    Pool.acquire,
    Pool.release,
    Pool.use,

    -- * Errors
    Pool.UsageError (..),
    Connection.ConnectionError,
    Session.SessionError (..),
    Session.CommandError (..),
    Session.ResultError (..),
    Session.RowError (..),

    -- * Session
    Session.Session,

    -- ** Session execution
    RunsSession (..),

    -- * Pipeline
    Pipeline.Pipeline,

    -- ** Pipeline execution
    RunsPipeline (..),

    -- * Transactions
    Transaction.Transaction,
    Transaction.Sessions.IsolationLevel (..),

    -- ** Transaction execution
    RunsTransaction (..),

    -- * Non-parameterized SQL
    RunsPlainSql (..),

    -- * Parametric statements
    Statement.Statement (..),

    -- ** Statement execution
    RunsStatement (..),
    runStatementByParams,

    -- ** Implicit statement definition
    IsStatementParams (..),
  )
where

import qualified Hasql.Connection as Connection
import qualified Hasql.Pipeline as Pipeline
import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Session
import qualified Hasql.Statement as Statement
import qualified Hasql.Transaction as Transaction
import qualified Hasql.Transaction.Sessions as Transaction.Sessions
import HasqlDev.Prelude

-- |
-- Capability of a functor to execute sessions.
class (Functor f) => RunsSession f where
  -- | Lift a session into the context of the functor.
  runSession :: Session.Session a -> f a

instance RunsSession Session.Session where
  runSession = id

instance RunsSession (ReaderT Connection.Connection (ExceptT Session.SessionError IO)) where
  runSession session = ReaderT \connection -> ExceptT (Session.run session connection)

-- | Capability of a functor to execute pipelines.
class (Functor f) => RunsPipeline f where
  -- | Lift a pipeline into the context of the functor.
  runPipeline :: Pipeline.Pipeline a -> f a

instance RunsPipeline Pipeline.Pipeline where
  runPipeline = id

instance RunsPipeline Session.Session where
  runPipeline = Session.pipeline

class (Functor f) => RunsTransaction f where
  -- | Lift a transaction into the context of the functor.
  runTransaction ::
    Transaction.Transaction a ->
    -- | Whether the transaction writes.
    Bool ->
    -- | Transaction isolation level.
    Transaction.Sessions.IsolationLevel ->
    f a

instance RunsTransaction Session.Session where
  runTransaction transaction isWrite isolationLevel =
    Transaction.Sessions.transaction isolationLevel mode transaction
    where
      mode =
        if isWrite
          then Transaction.Sessions.Write
          else Transaction.Sessions.Read

-- | Capability of a functor to execute unparameterized and possibly multistatement SQL-queries.
class (Functor f) => RunsPlainSql f where
  runPlainSql :: ByteString -> f ()

instance RunsPlainSql Session.Session where
  runPlainSql = Session.sql

-- |
-- Data structure modeling the statement parameters and determining the statement and its result type.
class IsStatementParams a where
  -- | The result type of a statement determined by its parameters.
  type StatementResultByParams a

  -- | Statement determined by its parameters.
  statementByParams :: Statement.Statement a (StatementResultByParams a)

-- |
-- Capability of a functor to execute statements.
class (Functor f) => RunsStatement f where
  -- | Execute a statement in the context of the functor, providing the parameters for it.
  runStatement :: Statement.Statement a b -> a -> f b

instance RunsStatement Pipeline.Pipeline where
  runStatement = flip Pipeline.statement

instance RunsStatement Session.Session where
  runStatement = flip Session.statement

instance RunsStatement Transaction.Transaction where
  runStatement = flip Transaction.statement

-- |
-- Execute a statement implicitly determined by its parameters in a functor that is capable of running statements.
runStatementByParams :: (RunsStatement f, IsStatementParams a) => a -> f (StatementResultByParams a)
runStatementByParams params = runStatement statementByParams params
