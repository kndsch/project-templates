{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module ProjectTemplates.App.Config
  ( AppConfig (..),
    templateConfigPath,
    templateFilePath,
    buildAppConfig,
    listAvailableTemplates,
    TemplateInfo (..),
  )
where

import Control.Monad.Catch (MonadThrow, throwM, try, SomeException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (LogLevel)
import Control.Monad.Logger.CallStack (LogLevel (..))
import Data.Yaml (decodeFileEither, ParseException)
import qualified Data.Text as T
import Path (Abs, Dir, File, Path, Rel, dirname, parseRelDir, parseSomeDir, relfile, toFilePath, (</>))
import Path.IO
  ( AnyPath (makeAbsolute),
    doesDirExist,
    listDir,
  )
import ProjectTemplates.App.Cli (Options (..))
import ProjectTemplates.App.Errors
import ProjectTemplates.Templates.Config (TemplateConfig(..))
import System.FilePath (dropTrailingPathSeparator)

data AppConfig = AppConfig
  { templateDir :: Path Abs Dir,
    current :: Bool,
    logLevel :: LogLevel
  }
  deriving (Show)

data TemplateInfo = TemplateInfo
  { templateName :: T.Text,
    templateDescription :: Maybe T.Text
  }
  deriving (Show)

templateFilePath :: Path Rel File -> AppConfig -> Path Abs File
templateFilePath file config = templateDir config </> file

templateConfigPath :: AppConfig -> Path Abs File
templateConfigPath config = templateDir config </> [relfile|config.yaml|]

buildAppConfig :: Options -> IO AppConfig
buildAppConfig Options {templatesDir, template, current, verbosity} = do
  templateDir <- case template of
    Just tmpl -> getTemplateDir templatesDir tmpl
    Nothing -> liftIO $ parseSomeDir templatesDir >>= makeAbsolute  -- For listing mode
  let logLevel = getLogLevel verbosity
  pure $
    AppConfig
      { templateDir = templateDir,
        current = current,
        logLevel = logLevel
      }

getTemplateDir :: (MonadIO m, MonadThrow m) => FilePath -> T.Text -> m (Path Abs Dir)
getTemplateDir dir template = do
  templatesDir' <- liftIO $ parseSomeDir dir >>= makeAbsolute
  templatesDirExists <- liftIO $ doesDirExist templatesDir'
  if not templatesDirExists
    then throwM $ RunTimeError "Templates directory does not exist"
    else do
      templateDir <- (templatesDir' </>) <$> parseRelDir (T.unpack template)
      templateDirExists <- doesDirExist templateDir
      if not templateDirExists
        then do
          availableTemplates <- fmap (dropTrailingPathSeparator . toFilePath . dirname) . fst <$> listDir templatesDir'
          throwM $ RunTimeError $ T.concat $ ["Template ", template, " not found. Available templates: "] ++ (T.pack <$> availableTemplates)
        else return templateDir

getLogLevel :: Int -> LogLevel
getLogLevel v = case v of
  0 -> LevelError
  1 -> LevelWarn
  2 -> LevelInfo
  _ -> LevelDebug

listAvailableTemplates :: (MonadIO m, MonadThrow m) => FilePath -> m [TemplateInfo]
listAvailableTemplates dir = do
  templatesDir' <- liftIO $ parseSomeDir dir >>= makeAbsolute
  templatesDirExists <- liftIO $ doesDirExist templatesDir'
  if not templatesDirExists
    then throwM $ RunTimeError "Templates directory does not exist"
    else do
      (templateDirs, _) <- listDir templatesDir'
      mapM getTemplateInfo templateDirs
  where
    getTemplateInfo :: (MonadIO m) => Path Abs Dir -> m TemplateInfo
    getTemplateInfo templateDir = do
      let templateName = T.pack $ dropTrailingPathSeparator $ toFilePath $ dirname templateDir
      let configPath = templateDir </> [relfile|config.yaml|]
      configResult <- liftIO $ (try :: IO (Either ParseException TemplateConfig) -> IO (Either SomeException (Either ParseException TemplateConfig))) $ decodeFileEither $ toFilePath configPath
      let templateDescription = case configResult of
            Right (Right config) -> description config
            _ -> Nothing
      return $ TemplateInfo templateName templateDescription
