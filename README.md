# effectful-sqlite

Effectful bindings for SQLite with connection pooling and migrations.

## Features

- Type-safe database effect using `effectful`
- Connection pooling via `resource-pool`
- Transaction support with proper connection reuse
- Migration system for schema versioning
- Re-exports common `sqlite-simple` types

## Usage

```haskell
import Effectful
import Effectful.Sqlite

main :: IO ()
main = do
  pool <- createPool "app.db" defaultPoolConfig
  result <- runEff $ runDB pool $ do
    execute_ "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, name TEXT)"
    execute "INSERT INTO users (name) VALUES (?)" (Only "Alice")
    query_ "SELECT id, name FROM users"
  print (result :: [(Int, Text)])
  destroyPool pool
```

## Migrations

```haskell
import Effectful.Sqlite

main :: IO ()
main = do
  pool <- createPool "app.db" defaultPoolConfig
  runEff $ runDB pool $ do
    result <- runMigrations "./migrations"
    case result of
      Left err -> error $ show err
      Right MigrationNoOp -> putStrLn "No migrations to run"
      Right (MigrationSuccess n) -> putStrLn $ "Ran " ++ show n ++ " migrations"
```
