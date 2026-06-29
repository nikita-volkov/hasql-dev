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
    Transaction.Sessions.Mode (..),

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
class (Monad f) => RunsSession f where
  -- | Lift a session into the context of the functor.
  runSession :: Session.Session a -> f a

instance RunsSession Session.Session where
  runSession = id

instance RunsSession (ReaderT Connection.Connection (ExceptT SessionError IO)) where
  runSession session = ReaderT \connection -> ExceptT (Connection.use connection session)

instance RunsSession (ReaderT Pool.Pool (ExceptT Pool.UsageError IO)) where
  runSession session = ReaderT \pool -> ExceptT (Pool.use pool session)

instance (RunsSession f) => RunsSession (StateT s f) where
  runSession session = StateT \s -> fmap (\a -> (a, s)) (runSession session)

instance (RunsSession f) => RunsSession (ReaderT r f) where
  runSession session = ReaderT \_ -> runSession session

instance (RunsSession f) => RunsSession (ExceptT e f) where
  runSession session = ExceptT (fmap Right (runSession session))

instance (RunsSession f, Monoid w) => RunsSession (WriterT w f) where
  runSession session = WriterT (fmap (\a -> (a, mempty)) (runSession session))

-- | Capability of a functor to execute pipelines.
class (Applicative f) => RunsPipeline f where
  -- | Lift a pipeline into the context of the functor.
  runPipeline :: Pipeline.Pipeline a -> f a

instance RunsPipeline Pipeline.Pipeline where
  runPipeline = id

instance RunsPipeline Session.Session where
  runPipeline = Session.pipeline

instance (Monad f, RunsPipeline f) => RunsPipeline (StateT s f) where
  runPipeline pipeline = StateT \s -> fmap (\a -> (a, s)) (runPipeline pipeline)

instance (Monad f, RunsPipeline f) => RunsPipeline (ReaderT r f) where
  runPipeline pipeline = ReaderT \_ -> runPipeline pipeline

instance (Monad f, RunsPipeline f) => RunsPipeline (ExceptT e f) where
  runPipeline pipeline = ExceptT (fmap Right (runPipeline pipeline))

instance (Monad f, RunsPipeline f, Monoid w) => RunsPipeline (WriterT w f) where
  runPipeline pipeline = WriterT (fmap (\a -> (a, mempty)) (runPipeline pipeline))

class (Monad f) => RunsTransaction f where
  -- | Lift a transaction into the context of the functor.
  runTransaction ::
    -- | Transaction isolation level.
    Transaction.Sessions.IsolationLevel ->
    -- | Transaction mode.
    Transaction.Sessions.Mode ->
    Transaction.Transaction a ->
    f a

instance RunsTransaction Session.Session where
  runTransaction = Transaction.Sessions.transaction

instance (RunsTransaction f) => RunsTransaction (StateT s f) where
  runTransaction isolationLevel mode transaction =
    StateT \s -> fmap (\a -> (a, s)) (runTransaction isolationLevel mode transaction)

instance (RunsTransaction f) => RunsTransaction (ReaderT r f) where
  runTransaction isolationLevel mode transaction =
    ReaderT \_ -> runTransaction isolationLevel mode transaction

instance (RunsTransaction f) => RunsTransaction (ExceptT e f) where
  runTransaction isolationLevel mode transaction =
    ExceptT (fmap Right (runTransaction isolationLevel mode transaction))

instance (RunsTransaction f, Monoid w) => RunsTransaction (WriterT w f) where
  runTransaction isolationLevel mode transaction =
    WriterT (fmap (\a -> (a, mempty)) (runTransaction isolationLevel mode transaction))

-- | Capability of a functor to execute unparameterized and possibly multistatement SQL-queries.
class (Monad f) => RunsScript f where
  -- | Execute an unparameterized and possibly multistatement SQL script in the context of the functor.
  runScript :: Text -> f ()

instance RunsScript Session.Session where
  runScript = Session.script

instance (RunsScript f) => RunsScript (StateT s f) where
  runScript sql = StateT \s -> fmap (\a -> (a, s)) (runScript sql)

instance (RunsScript f) => RunsScript (ReaderT r f) where
  runScript sql = ReaderT \_ -> runScript sql

instance (RunsScript f) => RunsScript (ExceptT e f) where
  runScript sql = ExceptT (fmap Right (runScript sql))

instance (RunsScript f, Monoid w) => RunsScript (WriterT w f) where
  runScript sql = WriterT (fmap (\a -> (a, mempty)) (runScript sql))

-- |
-- Capability of a functor to execute statements.
class (Applicative f) => RunsStatement f where
  -- | Execute a statement in the context of the functor, providing the parameters for it.
  runStatement :: Statement.Statement a b -> a -> f b

instance RunsStatement Pipeline.Pipeline where
  runStatement = flip Pipeline.statement

instance RunsStatement Session.Session where
  runStatement = flip Session.statement

instance RunsStatement Transaction.Transaction where
  runStatement = flip Transaction.statement

instance (Monad f, RunsStatement f) => RunsStatement (StateT s f) where
  runStatement statement params = StateT \s -> fmap (\a -> (a, s)) (runStatement statement params)

instance (Monad f, RunsStatement f) => RunsStatement (ReaderT r f) where
  runStatement statement params = ReaderT \_ -> runStatement statement params

instance (Monad f, RunsStatement f) => RunsStatement (ExceptT e f) where
  runStatement statement params = ExceptT (fmap Right (runStatement statement params))

instance (Monad f, RunsStatement f, Monoid w) => RunsStatement (WriterT w f) where
  runStatement statement params = WriterT (fmap (\a -> (a, mempty)) (runStatement statement params))

-- |
-- Execute a statement implicitly determined by its parameters in a functor that is capable of running statements.
runStatementByParams :: (RunsStatement f, Mapping.IsStatement params) => params -> f (Mapping.IsStatement.Result params)
runStatementByParams params = runStatement Mapping.IsStatement.statement params
