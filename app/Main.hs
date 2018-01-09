module Main where

import Text.Printf (printf)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer.Lazy

-- Recreation of https://gist.github.com/Decoherence/39a4c34685d86a334b63 with transformer library.

data Person = Person { name :: String } deriving Show

alex :: Person
alex = Person "Alex Fontaine"

philip :: Person
philip = Person "Philip Carpenter"

kim :: Person
kim = Person "Kim Lynch"

peopleDb :: [Person]
peopleDb = [alex, philip, kim]

process' :: ReaderT Person (WriterT String IO) String
process' = do
  _ <- lift . tell $ "Looking up Person... "
  Person p <- ask
  _ <- lift . tell $ printf "Found person: %s. " p
  return p

process :: ReaderT Person (WriterT String IO) ()
process = do
  _ <- lift . tell $ "Looking up Person. "
  Person p <- ask
  _ <- lift . tell $ printf "Found person: %s. " p
  (liftIO . putStrLn) p


main :: IO ()
main = do
  -- Print name from monad transformer
  result1 <- runWriterT (runReaderT process alex) -- :: ((), String)
  _ <- (putStrLn . snd) result1

  -- Extract the name from monad transfomer, then print it
  result2 <- runWriterT (runReaderT process' alex) -- :: (String, String)
  _ <- (putStrLn . fst) result2
  _ <- (putStrLn . snd) result2

  -- Now do the same thing for a list of people using mapM
  result3 <- runWriterT (mapM (runReaderT process') peopleDb) -- :: ([String], String)

  let people = fst result3
      log    = snd result3

  _ <- putStrLn "\n\nReaderT values:\n"
  _ <- mapM_ putStrLn people
  _ <- putStrLn "\nWriterT log:\n"
  putStrLn log
