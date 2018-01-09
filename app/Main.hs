module Main where

-- import Lib
import Text.Printf (printf)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer.Lazy

data Person = Person { name :: String } deriving Show

alex :: Person
alex = Person "Alex Fontaine"

philip :: Person
philip = Person "Philip Carpenter"

kim :: Person
kim = Person "Kim Lynch"

peopleDb :: [Person]
peopleDb = [alex, philip, kim]

process :: ReaderT Person (WriterT String IO) String
process = do
  _ <- lift . tell $ "Looking up Person"
  Person p <- ask
  _ <- lift . tell $ "Found person " ++ p
  return p

main :: IO ()
main = runProcess

runProcess :: IO ()
runProcess =
  let processed = mapM (runReaderT process) peopleDb
      written   = runWriterT processed
  in do (result, log) <- written
        _ <- putStrLn $ printf "result: %s" (show result)
        putStrLn $ printf "log: %s" log
