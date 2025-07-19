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
import ProjectTemplates.App.Cli (Options (..), cliParser)
import ProjectTemplates.App.Config (TemplateInfo (..), buildAppConfig, listAvailableTemplates)
import qualified ProjectTemplates.App.Config as Config
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
  useCurrent <- asks Config.current
  unless useCurrent $ preProcessHook config
  applyTemplate
  unless useCurrent $ postProcessHook config

execApp :: IO ()
execApp = handle showError $ do
  env <- readEnvVars
  options <- execParser $ cliParser env
  if listTemplates options
    then do
      templates <- listAvailableTemplates (templatesDir options)
      printTemplates templates
      exitSuccess
    else case template options of
      Nothing -> liftIO $ putStrLn "Error: TEMPLATE argument is required."
      Just _ -> do
        config <- buildAppConfig options
        void $ flip runReaderT config $ flip runStateT defaultAppState $ runApp app
        exitSuccess
  where
    showError :: (MonadIO m) => RunTimeError -> m ()
    showError (RunTimeError e) = liftIO $ putStrLn (T.unpack e)

    printTemplates :: [TemplateInfo] -> IO ()
    printTemplates templates = do
      putStrLn "Available templates:"
      mapM_ printTemplate templates

    printTemplate :: TemplateInfo -> IO ()
    printTemplate (TemplateInfo name (Just desc)) =
      putStrLn $ "  " ++ T.unpack name ++ " - " ++ T.unpack desc
    printTemplate (TemplateInfo name Nothing) =
      putStrLn $ "  " ++ T.unpack name
