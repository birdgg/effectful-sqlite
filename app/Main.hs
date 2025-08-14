{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Database.SQLite.Simple
import Effectful
import Effectful.Dispatch.Dynamic
import qualified Effectful.SQLite as ESQL
import Effectful.TH

data Todo = Todo {title :: String, done :: Bool} deriving (Show, Eq)

instance FromRow Todo where
  fromRow = Todo <$> field <*> field

instance ToRow Todo where
  toRow (Todo t d) = toRow (t, d)

data TodoRepository :: Effect where
  Create :: Todo -> TodoRepository m ()
  GetAll :: TodoRepository m [Todo]

makeEffect ''TodoRepository

runTodoRepository :: (ESQL.WithConnection :> es, IOE :> es) => Eff (TodoRepository : es) a -> Eff es a
runTodoRepository = interpret $ \_ -> \case
  Create todo -> ESQL.execute "INSERT INTO todos (title, done) VALUES (?, ?)" todo
  GetAll -> ESQL.query_ "SELECT title, done FROM todos"

main :: IO ()
main = do
  conn <- open "todos.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS todos (title TEXT, done BOOLEAN)"

  result <- runEff $ ESQL.runWithConnection conn $ runTodoRepository $ do
    create (Todo "Learn Haskell" False)
    create (Todo "Write SQLite wrapper" True)

    allTodos <- getAll
    liftIO $ putStrLn $ "All todos: " ++ show allTodos

  print result
  close conn
