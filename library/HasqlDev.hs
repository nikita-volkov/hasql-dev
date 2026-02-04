-- |
-- Capabilities-based algebra for integrating various abstractions of Hasql with the purpose of eradicating boilerplate code and making the experience smooth.
--
-- == Capabilities
--
-- Classes empowering a context with functionality.
--
-- E.g., with the help of the 'RunsStatement' typeclass 'Statement.Statement' can be executed in 'Session', 'Pipeline' and 'Transaction'.
--
-- Besides letting us connect different abstractions of Hasql together it also allows to integrate these functions directly into custom execution contexts like the main application-specific monad.
module HasqlDev
  ( -- * Connection Pool
    Pool.Pool,
    Pool.acquire,
    Pool.release,
    Pool.use,

    -- * Errors
    Pool.UsageError (..),
    module Hasql.Errors,

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
    RunsScript (..),

    -- * Parametric statements
    Statement.Statement,

    -- ** Statement execution
    RunsStatement (..),
    runStatementByParams,
  )
where

import qualified Hasql.Connection as Connection
import Hasql.Errors
import qualified Hasql.Mapping as Mapping
import qualified Hasql.Mapping.IsStatement as Mapping.IsStatement
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

instance RunsSession (ReaderT Connection.Connection (ExceptT SessionError IO)) where
  runSession session = ReaderT \connection -> ExceptT (Connection.use connection session)

instance RunsSession (ReaderT Pool.Pool (ExceptT Pool.UsageError IO)) where
  runSession session = ReaderT \pool -> ExceptT (Pool.use pool session)

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
class (Functor f) => RunsScript f where
  runScript :: Text -> f ()

instance RunsScript Session.Session where
  runScript = Session.script

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
runStatementByParams :: (RunsStatement f, Mapping.IsStatement params) => params -> f (Mapping.IsStatement.Result params)
runStatementByParams params = runStatement Mapping.IsStatement.statement params
