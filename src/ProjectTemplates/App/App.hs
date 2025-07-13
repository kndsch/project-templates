{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ProjectTemplates.App.App where

import Control.Monad (when)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Logger (MonadLogger (monadLoggerLog), ToLogStr (toLogStr), defaultOutput)
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.Reader.Class (asks)
import Control.Monad.State (StateT)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans
import ProjectTemplates.App.Config (AppConfig (..))
import ProjectTemplates.App.State (AppState (..))
import System.IO (stdout)

newtype AppT m a = AppT
  {runApp :: (StateT AppState (ReaderT AppConfig m)) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader AppConfig,
      MonadState AppState,
      MonadThrow,
      MonadCatch,
      MonadIO
    )

type App = AppT IO

instance (MonadIO m) => MonadLogger (AppT m) where
  monadLoggerLog loc src level msg = do
    l <- asks logLevel
    when (level >= l) $ liftIO $ defaultOutput stdout loc src level (toLogStr msg)
