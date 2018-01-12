module Config (readWriteConfig) where

import Text.Printf (printf)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer.Lazy
import qualified Data.Map.Lazy as Map
import Data.List (intercalate)

type Config = Map.Map String String

peopleConfig :: Config
peopleConfig = Map.fromList [("Alex", "Fontaine"), ("Philip", "Carpenter"), ("Kim", "Lynch")]

processNames :: ReaderT Config (WriterT String IO) ()
processNames = do
  _ <- (lift . tell) "Received the following names from config: "
  allNames <- ask
  let names = intercalate "," $ fmap (\el -> printf "%s %s" (fst el) (snd el)) (Map.toList allNames)
  (lift . tell) (show names)


readWriteConfig :: IO ()
readWriteConfig = do
  config <- runWriterT (runReaderT processNames peopleConfig) -- :: ((), [String])
  putStrLn $ printf "%s" (snd config)
