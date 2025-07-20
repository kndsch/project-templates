module ProjectTemplates.App.Process
  ( applyTemplate,
    processFile,
    processFileName,
    runProcessor,
  )
where

import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logDebugN, logInfoN, logWarnN)
import Control.Monad.Reader (asks)
import Control.Monad.State (gets)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Path
import Path.IO (createDirIfMissing, doesFileExist)
import ProjectTemplates.App.App
import ProjectTemplates.App.Config (templateFilePath)
import ProjectTemplates.App.Errors (InternalError (InternalError), RunTimeError (..))
import ProjectTemplates.App.Files (collectFiles, targetBaseDir)
import ProjectTemplates.App.State (AppState (..))
import ProjectTemplates.Templates.Config (TemplateError (..), overwrite)
import ProjectTemplates.Templates.Processor (TemplateProcessor, fillTemplate)
import System.FilePath (takeFileName)

applyTemplate :: App ()
applyTemplate = do
  files <- collectFiles
  mapM_ applyFile files

processFile :: TemplateProcessor a -> Path Rel File -> App a
processFile processor file = do
  fileAbs <- asks (templateFilePath file)
  logDebugN $ T.append "Processing file: " $ T.pack $ toFilePath fileAbs
  contents <- liftIO $ TIO.readFile $ toFilePath fileAbs
  runProcessor processor contents (Just fileAbs)

processFileName :: TemplateProcessor a -> Path Rel File -> App a
processFileName processor file = do
  fileAbs <- asks (templateFilePath file)
  let contents = T.pack $ toFilePath file
  logDebugN $ T.append "Processing file name: " contents
  runProcessor processor contents (Just fileAbs)

applyFile :: Path Rel File -> App ()
applyFile file = do
  defs' <- gets defs
  filledName <- processFileName (fillTemplate defs') file
  base <- targetBaseDir
  targetPath <- (base </>) <$> parseRelFile (T.unpack filledName)
  logInfoN $ T.append "Creating file: " $ T.pack $ toFilePath targetPath
  filledFile <- processFile (fillTemplate defs') file
  writeContents targetPath filledName filledFile

writeContents :: Path Abs File -> T.Text -> T.Text -> App ()
writeContents file relativePath contents = do
  exists <- doesFileExist file
  if exists
    then do
      overwriteFiles <- gets (overwrite . templateConfig)
      let fileName = T.pack $ takeFileName $ T.unpack relativePath
      let shouldOverwrite = fileName `elem` overwriteFiles
      if shouldOverwrite
        then do
          logInfoN $ T.append "Overwriting " relativePath
          createDirIfMissing True (parent file)
          liftIO $ TIO.writeFile (toFilePath file) contents
        else do
          logWarnN $ T.append "Skipping " relativePath
    else do
      createDirIfMissing True (parent file)
      liftIO $ TIO.writeFile (toFilePath file) contents

runProcessor :: TemplateProcessor a -> T.Text -> Maybe (Path Abs File) -> App a
runProcessor processor contents templatePath = either showError return $ processor contents
  where
    showError :: TemplateError -> App a
    showError err = case err of
      DefinitionNotFoundError _ -> throwM $ InternalError $ formatError err
      ParseError _ -> throwM $ RunTimeError $ formatError err
      UnsupportedTransform _ -> throwM $ RunTimeError $ formatError err
    formatError :: TemplateError -> T.Text
    formatError err = maybe "" (\t -> "(" <> (T.pack . toFilePath) t <> "): ") templatePath <> T.pack (show err)
