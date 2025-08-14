# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands

### Building
- `cabal build` - Build the library and executable
- `cabal build effectful-sqlite` - Build the library only  
- `cabal build effectful-sqlite:exe:effectful-sqlite` - Build the executable only

### Running
- `cabal run effectful-sqlite` - Run the demo application
- `cabal exec effectful-sqlite` - Alternative way to run the executable

### Development Tools
- `cabal repl` - Start GHCi REPL with the library loaded
- `cabal repl effectful-sqlite` - Start REPL for the library
- `cabal clean` - Clean build artifacts

## Project Architecture

This is a Haskell library that provides effectful bindings for SQLite using the `effectful` library. The architecture follows the effectful pattern for effect management.

### Core Architecture Patterns

**Effect Definition**: The library defines a `WithConnection` effect in `Effectful.SQLite.Connection` that provides access to SQLite connections within the effectful monad stack.

**Template Haskell Integration**: Uses `makeEffect` from `effectful-th` to automatically generate effect functions from GADT definitions.

**Interpreter Pattern**: Each effect has corresponding interpreter functions:
- `runWithConnection` - Runs effects with a single SQLite connection
- `runWithConnectionPool` - Runs effects with a connection pool (note: currently has PostgreSQL imports that need fixing)

**Lifted Operations**: The main module `Effectful.SQLite` provides lifted versions of `sqlite-simple` functions (`query`, `execute`, etc.) that work within the effectful monad.

### Module Structure

- `Effectful.SQLite` - Main module with lifted SQLite operations
- `Effectful.SQLite.Connection` - Core effect definition and single connection interpreter
- `Effectful.SQLite.Connection.Pool` - Connection pool interpreter (contains bugs - imports PostgreSQL instead of SQLite)

### Key Dependencies

- `effectful-core` - Core effectful library
- `effectful-th` - Template Haskell utilities for effects
- `sqlite-simple` - Underlying SQLite library

### Demo Application

The `app/Main.hs` demonstrates usage with a Todo application that shows:
- Effect composition (TodoRepository effect built on top of WithConnection)
- Error handling with exception catching
- Basic CRUD operations
- Database initialization

### Known Issues

The connection pool module (`Effectful.SQLite.Connection.Pool`) has incorrect imports referencing PostgreSQL instead of SQLite - this will cause compilation errors if used.