{-# LANGUAGE BlockArguments #-}

module ProjectTemplates.Run
  ( execApp,
  )
where

import Control.Monad (unless, void)
import Control.Monad.Catch (handle)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
  ( ReaderT (runReaderT),
  )
import Control.Monad.Reader.Class (asks)
import Control.Monad.State (runStateT)
import qualified Data.Text as T
import Options.Applicative (execParser)
import ProjectTemplates.App.App (App, AppT (runApp))
import ProjectTemplates.App.Cli (cliParser)
import ProjectTemplates.App.Config (AppConfig (..), buildAppConfig)
import ProjectTemplates.App.Env (readEnvVars)
import ProjectTemplates.App.Errors (RunTimeError (..))
import ProjectTemplates.App.Files (readConfig)
import ProjectTemplates.App.Hooks (postProcessHook, preProcessHook)
import ProjectTemplates.App.Process (applyTemplate)
import ProjectTemplates.App.State (defaultAppState)
import ProjectTemplates.App.Variables (getVariableDefinitions)
import System.Exit (exitSuccess)

app :: App ()
app = do
  config <- readConfig
  getVariableDefinitions config
  useCurrent <- asks current
  unless useCurrent $ preProcessHook config
  applyTemplate
  unless useCurrent $ postProcessHook config


execApp :: IO ()
execApp = handle showError $ do
  config <- getAppConfig
  void $ flip runReaderT config $ flip runStateT defaultAppState $ runApp app
  exitSuccess
  where
    showError :: (MonadIO m) => RunTimeError -> m ()
    showError (RunTimeError e) = liftIO $ putStrLn (T.unpack e)

getAppConfig :: IO AppConfig
getAppConfig = do
  env <- liftIO readEnvVars
  options <- liftIO $ execParser $ cliParser env
  buildAppConfig options
