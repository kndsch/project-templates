{-# LANGUAGE FlexibleContexts #-}

module ProjectTemplates.App.State where

import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.State (MonadState)
import Control.Monad.State.Class (gets)
import qualified Data.Map as M
import qualified Data.Text as T
import ProjectTemplates.App.Errors (InternalError (..))
import ProjectTemplates.Templates.Config (VariableDefinitions)

newtype AppState = AppState
  { defs ::
      VariableDefinitions
  }

defaultAppState :: AppState
defaultAppState = AppState M.empty

projectNameKey :: T.Text
projectNameKey = "project-name"

getProjectName :: (MonadState AppState m, MonadThrow m) => m T.Text
getProjectName = do
  name <- gets $ M.lookup projectNameKey . defs
  case name of
    Just n -> return n
    Nothing -> throwM $ InternalError $ "Project name not found in definitions: " <> projectNameKey
