{-# LANGUAGE RecordWildCards #-}

module ProjectTemplates.App.Env where

import Data.Maybe (fromMaybe)
import System.Directory
  ( XdgDirectory (XdgConfig),
    getXdgDirectory,
  )
import System.Environment (lookupEnv)

newtype EnvVars = EnvVars {defaultTemplatesDir :: FilePath}

readEnvVars :: IO EnvVars
readEnvVars = do
  xdgTemplateDirectory <- getXdgDirectory XdgConfig "project-templates"
  defaultTemplatesDir <- fromMaybe xdgTemplateDirectory <$> lookupEnv "TEMPLATES_DIR"
  return EnvVars {..}
