module ProjectTemplates.App.Files
  ( readConfig,
    collectFiles,
    targetDir,
    targetBaseDir,
    workingDir,
  )
where

import Control.Monad.Catch (handle, throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Either (isRight)
import qualified Data.Text as T
import qualified Data.Yaml as Y
import Path
import Path.IO
import ProjectTemplates.App.App (App)
import ProjectTemplates.App.Config
  ( AppConfig (..),
    templateConfigPath,
    templateDir,
  )
import ProjectTemplates.App.Errors (RunTimeError (..))
import ProjectTemplates.App.State (getProjectName)
import ProjectTemplates.Templates.Config (TemplateConfig (..), defaultTemplateConfig)
import ProjectTemplates.Templates.Processor (parseValueDescription)

readConfig :: App TemplateConfig
readConfig = do
  configPath <- asks templateConfigPath
  fileExists <- doesFileExist configPath
  if not fileExists
    then return defaultTemplateConfig
    else readConfigYaml configPath

readConfigYaml :: Path Abs File -> App TemplateConfig
readConfigYaml path = do
  handle mapException $ liftIO $ Y.decodeFileThrow (toFilePath path)
  where
    mapException :: Y.ParseException -> App a
    mapException = throwM . RunTimeError . T.pack . show

collectFiles :: App [Path Rel File]
collectFiles = do
  dir <- asks templateDir
  templateSubdir <- getTemplateSubdir dir
  files <- snd <$> listDirRecur templateSubdir
  mapM (stripProperPrefix dir) files

getTemplateSubdir :: Path Abs Dir -> App (Path Abs Dir)
getTemplateSubdir dir = do
  subdirs <- filter isTemplateSubDir . fst <$> listDir dir
  case subdirs of
    [d] -> return d
    [] -> throwM $ RunTimeError $ "No template subdirectory found in " <> T.pack (toFilePath dir)
    (_ : _) -> throwM $ RunTimeError $ "Multiple template subdirectories found in " <> T.pack (toFilePath dir)

isTemplateSubDir :: Path Abs Dir -> Bool
isTemplateSubDir = isRight . parseValueDescription . T.pack . toFilePath . dirname

workingDir :: App (Path Abs Dir)
workingDir = liftIO getCurrentDir

targetBaseDir :: App (Path Abs Dir)
targetBaseDir = do
  useCurrent <- asks current
  if not useCurrent
    then workingDir
    else parent <$> workingDir

targetDir :: App (Path Abs Dir)
targetDir = do
  relProjectDir <- parseRelDir . T.unpack =<< getProjectName
  fmap (</> relProjectDir) targetBaseDir
