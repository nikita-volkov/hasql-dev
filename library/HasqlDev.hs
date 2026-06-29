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
-- Capability of a monad to execute sessions.
--
-- 'runSession' is a monad morphism from 'Session.Session':
--
-- @
-- runSession ('pure' a) = 'pure' a
-- runSession (m '>>=' k) = runSession m '>>=' runSession . k
-- @
class (Monad f) => RunsSession f where
  -- | Lift a session into the context of the monad.
  runSession :: Session.Session a -> f a

instance RunsSession Session.Session where
  runSession = id

-- | Capability of an applicative functor to execute pipelines.
--
-- 'runPipeline' is an applicative morphism from 'Pipeline.Pipeline':
--
-- @
-- runPipeline ('pure' a) = 'pure' a
-- runPipeline (pf '<*>' px) = runPipeline pf '<*>' runPipeline px
-- @
class (Applicative f) => RunsPipeline f where
  -- | Lift a pipeline into the context of the functor.
  runPipeline :: Pipeline.Pipeline a -> f a

instance RunsPipeline Pipeline.Pipeline where
  runPipeline = id

instance RunsPipeline Session.Session where
  runPipeline = Session.pipeline

-- |
-- Capability of a monad to execute transactions.
--
-- For fixed isolation level and mode, 'runTransaction' is a monad morphism
-- from 'Transaction.Transaction':
--
-- @
-- runTransaction lvl mode ('pure' a) = 'pure' a
-- runTransaction lvl mode (m '>>=' k) =
--   runTransaction lvl mode m '>>=' runTransaction lvl mode . k
-- @
class (RunsSession f) => RunsTransaction f where
  -- | Lift a transaction into the context of the monad.
  runTransaction ::
    -- | Transaction isolation level.
    Transaction.Sessions.IsolationLevel ->
    -- | Transaction mode.
    Transaction.Sessions.Mode ->
    Transaction.Transaction a ->
    f a
  default runTransaction ::
    -- | Transaction isolation level.
    Transaction.Sessions.IsolationLevel ->
    -- | Transaction mode.
    Transaction.Sessions.Mode ->
    Transaction.Transaction a ->
    f a
  runTransaction isolationLevel mode transaction =
    runSession (Transaction.Sessions.transaction isolationLevel mode transaction)

instance RunsTransaction Session.Session where
  runTransaction = Transaction.Sessions.transaction

-- | Capability of a monad to execute unparameterized and possibly multistatement SQL-queries.
--
-- Law: @runScript sql = runSession (Session.script sql)@
class (Monad f) => RunsScript f where
  -- | Execute an unparameterized and possibly multistatement SQL script in the context of the monad.
  runScript :: Text -> f ()

instance RunsScript Session.Session where
  runScript = Session.script

-- |
-- Capability of an applicative functor to execute statements.
--
-- When the functor is also an instance of 'RunsSession' or 'RunsPipeline',
-- 'runStatement' should be consistent with them:
--
-- @
-- runStatement stmt params = runSession (Session.statement params stmt)
-- runStatement stmt params = runPipeline (Pipeline.statement params stmt)
-- @
class (Applicative f) => RunsStatement f where
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
