{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module ProjectTemplates.App.Variables (setVariableDefinitions) where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Loops (untilJust)
import Control.Monad.RWS (modify)
import Control.Monad.Reader (asks)
import Data.Bool (bool)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Path
import Path.IO (getCurrentDir)
import ProjectTemplates.App.App
import ProjectTemplates.App.Config
import ProjectTemplates.App.Files (collectFiles)
import ProjectTemplates.App.Process
import ProjectTemplates.App.State (AppState (..), projectNameKey)
import ProjectTemplates.Templates.Config (TemplateConfig (..), VariableConfig (value), VariableDefinitions, getDefaultValue)
import ProjectTemplates.Templates.Processor (collectVariables)
import System.Console.Haskeline (defaultSettings, getInputLine, runInputT)
import System.FilePath (dropTrailingPathSeparator)

createDefinitions :: TemplateConfig -> App VariableDefinitions
createDefinitions config = do
  let defs' = M.mapMaybe value (variables config)
  asks current >>= bool (return defs') (addProjectName defs')
  where
    addProjectName :: VariableDefinitions -> App VariableDefinitions
    addProjectName defs' = do
      currentDir <- liftIO getCurrentDir
      let projectName = (T.pack . dropTrailingPathSeparator . toFilePath . dirname) currentDir
      return $ M.insert projectNameKey projectName defs'

collectAllVariables :: TemplateConfig -> App (S.Set T.Text)
collectAllVariables config = do
  files <- collectFiles
  fileNameVars <- concat <$> mapM (processFileName collectVariables) files
  fileVars <- concat <$> mapM (processFile collectVariables) files
  let hooks = preProcess config ++ postProcess config
  hookVars <- concat <$> mapM (\h -> runProcessor collectVariables h Nothing) hooks
  return $ S.fromList (fileNameVars ++ fileVars ++ hookVars ++ [projectNameKey])

setVariableDefinitions :: TemplateConfig -> App ()
setVariableDefinitions config = do
  usedVariables <- collectAllVariables config
  configDefinitions <- createDefinitions config
  let neededDefinitions = S.toList $ usedVariables `S.difference` M.keysSet configDefinitions
  newDefinitions <- liftIO $ mapM (requestVariable config) neededDefinitions
  modify $ \s -> s {defs = M.union configDefinitions (M.fromList newDefinitions)}

requestVariable :: TemplateConfig -> T.Text -> IO (T.Text, T.Text)
requestVariable config var = do
  let defaultValue' = getDefaultValue config var
  def <- readDefinition (T.concat [prompt, defaultPrompt defaultValue', ": "]) defaultValue'
  return (var, def)
  where
    prompt = T.concat ["Enter value for variable '", var, "'"]
    defaultPrompt = maybe "" (\v -> T.concat [" (default: ", v, ")"])

readDefinition :: T.Text -> Maybe T.Text -> IO T.Text
readDefinition prompt defaultValue' = untilJust $ do
  def <- fmap T.pack <$> runInputT defaultSettings (getInputLine (T.unpack prompt))
  return $ def <|> defaultValue'
