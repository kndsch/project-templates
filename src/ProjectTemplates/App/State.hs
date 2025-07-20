{-# LANGUAGE FlexibleContexts #-}

module ProjectTemplates.App.State
  ( AppState (..),
    defaultAppState,
    projectNameKey,
    getProjectName,
  )
where

import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.State (MonadState)
import Control.Monad.State.Class (gets)
import qualified Data.Map as M
import qualified Data.Text as T
import ProjectTemplates.App.Errors (InternalError (..))
import ProjectTemplates.Templates.Config (TemplateConfig, VariableDefinitions, defaultTemplateConfig)

data AppState = AppState
  { defs :: VariableDefinitions,
    templateConfig :: TemplateConfig
  }

defaultAppState :: AppState
defaultAppState = AppState M.empty defaultTemplateConfig

projectNameKey :: T.Text
projectNameKey = "project-name"

getProjectName :: (MonadState AppState m, MonadThrow m) => m T.Text
getProjectName = do
  name <- gets $ M.lookup projectNameKey . defs
  case name of
    Just n -> return n
    Nothing -> throwM $ InternalError $ "Project name not found in definitions: " <> projectNameKey
