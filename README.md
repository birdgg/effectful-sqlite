# effectful-sqlite

Effectful bindings for SQLite with migrations support.

## Features

- Type-safe `SQLite` effect using `effectful`
- Connection pooling via `resource-pool` (user-managed)
- Multiple handler modes: pool, single connection, or auto-managed
- Streaming operations for memory-efficient processing
- Enhanced transaction support: savepoints, immediate/exclusive modes
- Migration system for schema versioning
- Re-exports common `sqlite-simple` types

## Installation

```cabal
build-depends: effectful-sqlite
```

## Usage

### Basic Usage with Connection Pool

```haskell
import Data.Pool qualified as Pool
import Database.SQLite.Simple qualified as SQL
import Effectful
import Effectful.Sqlite

main :: IO ()
main = do
  -- Create pool using resource-pool
  pool <- Pool.newPool $ Pool.defaultPoolConfig
    (SQL.open "app.db")
    SQL.close
    300  -- idle time (seconds)
    10   -- max connections

  result <- runEff $ runSQLite pool $ do
    execute_ "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, name TEXT)"
    execute "INSERT INTO users (name) VALUES (?)" (Only "Alice")
    query_ "SELECT id, name FROM users"

  print (result :: [(Int, String)])
  Pool.destroyAllResources pool
```

### Simple Usage with Path

```haskell
main :: IO ()
main = runEff $ runSQLiteWithPath "app.db" $ do
  execute_ "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, name TEXT)"
  rows <- query_ "SELECT * FROM users"
  liftIO $ print (rows :: [(Int, String)])
```

### Streaming Operations

Process large result sets without loading everything into memory:

```haskell
-- Fold over results
total <- fold "SELECT amount FROM transactions" () 0 $ \acc row ->
  pure (acc + row)

-- Execute action for each row
forEach_ "SELECT name FROM users" $ \(Only name) ->
  liftIO $ putStrLn name
```

### Transactions

```haskell
-- Basic transaction
withTransaction $ do
  execute "UPDATE accounts SET balance = balance - ?" (Only amount)
  execute "INSERT INTO log (msg) VALUES (?)" (Only "transfer")

-- Savepoint (nested transaction)
withTransaction $ do
  execute "INSERT INTO users (name) VALUES (?)" (Only "Alice")
  withSavepoint $ do
    execute "INSERT INTO profiles (user_id) VALUES (?)" (Only odbc1)
    -- If this fails, only the savepoint is rolled back

-- Exclusive transaction (for writes)
withExclusiveTransaction $ do
  execute "UPDATE counters SET value = value + 1" ()
```

### Execute with Result

Get the number of changed rows and last inserted row ID:

```haskell
result <- executeReturning "INSERT INTO users (name) VALUES (?)" (Only "Bob")
print result.changedRows  -- 1
print result.lastRowId    -- e.g., 42
```

## Migrations

Place SQL files in a directory with naming format `{version}_{name}.sql`:

```
migrations/
  20240101000000_create_users.sql
  20240102000000_add_email.sql
```

```haskell
main :: IO ()
main = runEff $ runSQLiteWithPath "app.db" $ do
  result <- runMigrations "./migrations"
  case result of
    Left err -> liftIO $ print err
    Right MigrationNoOp -> liftIO $ putStrLn "No migrations to run"
    Right (MigrationSuccess n) -> liftIO $ putStrLn $ "Ran " ++ show n ++ " migrations"
```

## API Overview

### Handlers

| Handler | Description |
|---------|-------------|
| `runSQLite` | Use with connection pool |
| `runSQLiteWithConnection` | Use with single connection |
| `runSQLiteWithPath` | Auto-manage connection from path |

### Operations

- **Query**: `query`, `query_`, `queryNamed`
- **Execute**: `execute`, `execute_`, `executeMany`, `executeNamed`
- **Execute with Result**: `executeReturning`, `executeReturning_`
- **Streaming**: `fold`, `fold_`, `foldNamed`, `forEach`, `forEach_`, `forEachNamed`
- **Transaction**: `withTransaction`, `withSavepoint`, `withImmediateTransaction`, `withExclusiveTransaction`, `begin`, `commit`, `rollback`
- **Migration**: `runMigrations`, `getMigrationStatus`, `getPendingMigrations`
- **Low-level**: `withConnection`

## License

MIT
