module Config (readWriteConfig) where

import Text.Printf (printf)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer.Lazy
import qualified Data.Map.Lazy as Map
import Data.List (intercalate)
import Data.Functor.Identity (runIdentity)
import Text.Read (readMaybe)

type Config = Map.Map String String

serverConfig :: Config
serverConfig = Map.fromList [("host", "localhost"), ("port", "7654")]

getHost :: Reader Config (Maybe String)
getHost = do
  config <- ask
  return (Map.lookup "host" config)

getPort :: Reader Config (Maybe Int)
getPort = do
  config <- ask
  return (Map.lookup "port" config >>= readMaybe)

getConfig :: ReaderT Config (WriterT String IO) ()
getConfig = do
  hostM <- mapReaderT return getHost
  portM <- mapReaderT return getPort
  let host = maybe "-" id (runIdentity hostM)
      port = maybe "-" show (runIdentity portM)
  _ <- (lift . tell) $ "\nConfig"
  _ <- (lift . tell) $ "\n======"
  _ <- (lift . tell) $ printf "\nhost: %s" host
  _ <- (lift . tell) $ printf "\nport: %s" port
  return ()

readWriteConfig :: IO ()
readWriteConfig = execWriterT (runReaderT getConfig serverConfig) >>= putStrLn
