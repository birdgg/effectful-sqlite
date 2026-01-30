# effectful-sqlite

Type-safe SQLite bindings for effectful with compile-time transaction safety.

## Build Commands

```bash
cabal build           # Build project
cabal haddock         # Generate Haddock docs
cabal clean           # Clean build artifacts
```

## Architecture

### Two-Layer Effect Design

The library uses two effects to enforce transaction boundaries at compile time:

| Effect | Dispatch | Purpose |
|--------|----------|---------|
| `Sqlite` | `'Static 'WithSideEffects` | Connection pool management |
| `SqliteTransaction` | `'Static 'NoSideEffects` | Database operations |

All database operations require `SqliteTransaction`, which can only be introduced via:

- `transact` - DEFERRED transaction
- `transactImmediate` - IMMEDIATE transaction
- `transactExclusive` - EXCLUSIVE transaction
- `notransact` - Auto-commit mode

### Effect Stack

```haskell
runEff . runLog "app" logger . runConcurrent . runSqlite (DbFile "app.db") $ do
  transact $ execute "INSERT INTO users (name) VALUES (?)" (Only "Alice")
```

Required effects for transaction functions:
- `Sqlite :> es`
- `Concurrent :> es`
- `Log :> es`
- `IOE :> es`

### Automatic Retry

All transaction functions (`transact`, `notransact`, etc.) automatically retry on `ErrorBusy` with exponential backoff (max 10 retries). Retry events are logged via `Log` effect.

### Connection Pool

Default pool settings (via `withSqlPool`):
- `PRAGMA busy_timeout=3000` on each connection
- Idle timeout: 0.5 seconds
- Connection count based on CPU capabilities

## Key Modules

| Module | Description |
|--------|-------------|
| `Effectful.Sqlite` | Main entry, re-exports public API |
| `Effectful.Sqlite.Effect` | Effect definitions and handlers |
| `Effectful.Sqlite.Types` | `SqliteDb`, `SqlitePool` types |
| `Effectful.Sqlite.Migration.Types` | Migration types |
| `Effectful.Sqlite.Migration.Runner` | Migration execution |

## API Summary

### Handlers

| Handler | Description |
|---------|-------------|
| `runSqlite` | Run without SQL logging |
| `runSqliteDebug` | Run with SQL logging (`String -> IO ()`) |

### Transaction Boundaries

| Function | Mode |
|----------|------|
| `transact` | DEFERRED (default) |
| `transactImmediate` | IMMEDIATE (write lock immediately) |
| `transactExclusive` | EXCLUSIVE (blocks all access) |
| `notransact` | Auto-commit |
| `savepoint` | Nested transaction |

### Operations

- **Query**: `query`, `query_`, `queryNamed`
- **Execute**: `execute`, `execute_`, `executeMany`, `executeNamed`, `executeNamedReturningChanges`
- **Migration**: `runMigrations`, `getMigrationStatus`, `getPendingMigrations`

### Insert with RETURNING

Use SQLite's `RETURNING` clause (3.35+) to get inserted data:

```haskell
users <- query "INSERT INTO users (name) VALUES (?) RETURNING *" (Only "Alice")
```

## Tech Stack

- **Language**: GHC2024
- **Effect framework**: effectful + effectful-core
- **Database**: sqlite-simple
- **Pool**: resource-pool
- **Logging**: log-effectful

## Inspired By

Architecture based on [beam-sqlite-effectful](https://github.com/deepflowinc-oss/effectful-extras/tree/main/beam-sqlite-effectful).
