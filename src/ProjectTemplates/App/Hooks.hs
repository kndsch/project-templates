module ProjectTemplates.App.Hooks (preProcessHook, postProcessHook) where

import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logInfoN)
import Control.Monad.State (gets)
import qualified Data.Text as T
import Path (Abs, Dir, Path, toFilePath)
import Path.IO ()
import ProjectTemplates.App.App (App)
import ProjectTemplates.App.Errors (RunTimeError (..))
import ProjectTemplates.App.Files (targetDir, workingDir)
import ProjectTemplates.App.Process (runProcessor)
import ProjectTemplates.App.State (defs)
import ProjectTemplates.Templates.Config (TemplateConfig (..))
import ProjectTemplates.Templates.Processor (fillTemplate)
import System.Exit (ExitCode (ExitSuccess))
import System.Process (createProcess, shell, waitForProcess)

preProcessHook :: TemplateConfig -> App ()
preProcessHook config = do
  processDir <- workingDir
  let hooks = preProcess config
  mapM_ (runCatchError processDir) hooks

postProcessHook :: TemplateConfig -> App ()
postProcessHook config = do
  processDir <- targetDir
  let hooks = postProcess config
  mapM_ (runCatchError processDir) hooks

runCatchError :: Path Abs Dir -> T.Text -> App ()
runCatchError processDir cmdTemplate = do
  defs' <- gets defs
  cmd <- runProcessor (fillTemplate defs') cmdTemplate Nothing
  logInfoN $ T.append "Running command: " cmd
  let p = shell $ "cd " ++ toFilePath processDir ++ " && " ++ T.unpack cmd
  (_, _, _, ph) <- liftIO $ createProcess p
  exitCode <- liftIO $ waitForProcess ph
  if exitCode == ExitSuccess
    then return ()
    else throwM $ RunTimeError $ "Error running command: " <> cmd
