# effectful-sqlite

为 SQLite 数据库提供基于 effectful 框架的类型安全绑定，包含迁移系统。

## 构建和开发命令

```bash
cabal build           # 构建项目
cabal haddock         # 生成 Haddock 文档
cabal clean           # 清理构建产物
```

## 架构概述

### Effect 系统设计

核心 `SQLite` Effect 使用 GADT 定义，支持以下操作：

- **查询操作**：`query`, `query_`, `queryNamed`
- **执行操作**：`execute`, `execute_`, `executeMany`, `executeNamed`, `executeNamedReturningChanges`
- **执行结果**：`executeReturning`, `executeReturning_`（返回 `ExecuteResult`，包含 `changedRows` 和 `lastRowId`）
- **流式操作**：`fold`, `fold_`, `foldNamed`, `forEach`, `forEach_`, `forEachNamed`
- **事务控制**：
  - 自动事务：`withTransaction`, `withSavepoint`, `withImmediateTransaction`, `withExclusiveTransaction`
  - 手动事务：`begin`, `commit`, `rollback`
- **迁移**：`runMigrations`, `getMigrationStatus`, `getPendingMigrations`, `createMigrationsTable`
- **底层访问**：`withConnection`

### Handler 模式

提供三种运行方式：

| Handler | 用途 |
|---------|------|
| `runSQLite` | 使用连接池（生产环境） |
| `runSQLiteWithConnection` | 使用单个连接（测试） |
| `runSQLiteWithPath` | 自动管理连接（一次性脚本） |

### 连接池管理

连接池由用户通过 `resource-pool` 库创建，传入 `runDB`：

```haskell
import Data.Pool qualified as Pool
import Database.SQLite.Simple qualified as SQL

pool <- Pool.newPool $ Pool.defaultPoolConfig
  (SQL.open "test.db")
  SQL.close
  300   -- idle time (seconds)
  10    -- max connections

runSQLite pool $ do
  rows <- query_ "SELECT * FROM users"
  ...
```

通过 `TxContext`（`IORef (Maybe Connection)`）实现事务内连接复用：
- 事务外：每次操作从池中获取连接
- 事务内：复用同一连接，确保原子性

### 迁移系统

迁移文件命名格式：`{version}_{name}.sql`（如 `20240114120000_create_users.sql`）

迁移记录存储在 `_migrations` 表中，追踪版本和执行时间。

## 关键模块

| 模块 | 描述 |
|------|------|
| `Effectful.Sqlite` | 主入口，重导出所有公共 API |
| `Effectful.Sqlite.Effect` | DB Effect 定义和 handler |
| `Effectful.Sqlite.Types` | ExecuteResult 类型定义 |
| `Effectful.Sqlite.Internal` | 内部连接管理（TxContext） |
| `Effectful.Sqlite.Migration.Types` | 迁移相关类型定义 |
| `Effectful.Sqlite.Migration.Runner` | 迁移执行逻辑 |

## 技术规范

- **语言**: GHC2024
- **Effect 框架**: effectful + effectful-th
- **数据库绑定**: sqlite-simple
- **连接池**: resource-pool（用户自行管理）

## 常用扩展

项目默认启用以下扩展：
- `DataKinds`, `GADTs`, `TypeFamilies`
- `DerivingStrategies`
- `OverloadedStrings`, `OverloadedRecordDot`
- `LambdaCase`, `DuplicateRecordFields`
