# effectful-sqlite

Type-safe SQLite bindings for effectful with compile-time transaction safety.

## Features

- Two-layer effect design enforcing transaction boundaries at compile time
- Connection pooling with automatic retry on `ErrorBusy`
- Multiple transaction modes: DEFERRED, IMMEDIATE, EXCLUSIVE
- Savepoints for nested transactions
- SQL query logging for debugging
- Migration system for schema versioning
- Re-exports common `sqlite-simple` types

## Installation

```cabal
build-depends: effectful-sqlite
```

## Quick Start

```haskell
import Effectful
import Effectful.Sqlite

main :: IO ()
main = runEff . runLog "app" logger . runConcurrent . runSqlite (DbFile "app.db") $ do
  -- Run migrations
  runMigrations "migrations/"

  -- Query without transaction (auto-commit)
  users <- notransact $ query_ "SELECT id, name FROM users"
  liftIO $ print users

  -- Insert with transaction
  transact $ do
    execute "INSERT INTO users (name) VALUES (?)" (Only "Alice")
```

## Architecture

The library uses a two-layer effect design:

- `Sqlite` - Outer layer for connection pool management
- `SqliteTransaction` - Inner layer for database operations

All database operations require `SqliteTransaction`, which can only be introduced via:

- `transact` - DEFERRED transaction
- `transactImmediate` - IMMEDIATE transaction (acquires write lock immediately)
- `transactExclusive` - EXCLUSIVE transaction (blocks all access)
- `notransact` - Auto-commit mode

This ensures transaction boundaries are explicit and enforced at compile time.

## Usage

### Transactions

```haskell
-- DEFERRED (default) - lock acquired on first read/write
transact $ do
  execute "INSERT INTO users (name) VALUES (?)" (Only "Alice")
  execute "INSERT INTO logs (msg) VALUES (?)" (Only "User created")

-- IMMEDIATE - acquire write lock immediately
transactImmediate $ do
  execute "UPDATE accounts SET balance = balance - 100 WHERE id = ?" (Only 1)
  execute "UPDATE accounts SET balance = balance + 100 WHERE id = ?" (Only 2)

-- EXCLUSIVE - block all other access
transactExclusive $ do
  execute "VACUUM"
```

### Savepoints (Nested Transactions)

```haskell
transact $ do
  execute "INSERT INTO users (name) VALUES (?)" (Only "Alice")

  savepoint $ do
    execute "INSERT INTO profiles (user_id) VALUES (?)" (Only odbc1)
    -- If this fails, only the savepoint is rolled back
    -- Alice's insert is preserved
```

### Insert with RETURNING

```haskell
-- Get inserted data using RETURNING clause (SQLite 3.35+)
users <- transact $
  query "INSERT INTO users (name) VALUES (?) RETURNING *" (Only "Alice")
```

### Debug Logging

```haskell
-- Log all SQL queries to stdout
runSqliteDebug putStrLn (DbFile "app.db") $ do
  transact $ execute "INSERT INTO users (name) VALUES (?)" (Only "Alice")
  -- Prints: "INSERT INTO users (name) VALUES (?)"
```

### Custom Connection Pool

```haskell
import Data.Pool qualified as Pool
import Database.SQLite.Simple qualified as SQL

main :: IO ()
main = do
  pool <- fmap SqlitePool $ Pool.newPool $ Pool.defaultPoolConfig
    (SQL.open "app.db")
    SQL.close
    0.5   -- idle timeout (seconds)
    10    -- max connections

  runEff . runLog "app" logger . runConcurrent . runSqlite (DbPool pool) $ do
    transact $ execute_ "SELECT 1"
```

## Migrations

Place SQL files in a directory with naming format `{version}_{name}.sql`:

```
migrations/
  20240101000000_create_users.sql
  20240102000000_add_email.sql
```

```haskell
main = runEff . runLog "app" logger . runConcurrent . runSqlite (DbFile "app.db") $ do
  result <- runMigrations "migrations/"
  case result of
    Left err -> liftIO $ print err
    Right MigrationNoOp -> liftIO $ putStrLn "No migrations to run"
    Right (MigrationSuccess n) -> liftIO $ putStrLn $ "Ran " ++ show n ++ " migrations"
```

## API Overview

### Handlers

| Handler | Description |
|---------|-------------|
| `runSqlite` | Run without SQL logging |
| `runSqliteDebug` | Run with SQL logging |

### Transaction Boundaries

| Function | Description |
|----------|-------------|
| `transact` | DEFERRED transaction |
| `transactImmediate` | IMMEDIATE transaction |
| `transactExclusive` | EXCLUSIVE transaction |
| `notransact` | Auto-commit mode |
| `savepoint` | Nested transaction |

### Operations

| Category | Functions |
|----------|-----------|
| Query | `query`, `query_`, `queryNamed` |
| Execute | `execute`, `execute_`, `executeMany`, `executeNamed`, `executeNamedReturningChanges` |
| Migration | `runMigrations`, `getMigrationStatus`, `getPendingMigrations` |

## Acknowledgments

This library's architecture is inspired by [beam-sqlite-effectful](https://github.com/deepflowinc-oss/effectful-extras/tree/main/beam-sqlite-effectful).

## License

MIT
