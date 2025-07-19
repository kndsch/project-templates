{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ConfigSpec (spec) where

import Control.Exception (throwIO)
import Control.Monad (forM_)
import Control.Monad.Catch (try)
import Data.Text (Text)
import qualified Data.Text as T
import Path (Abs, Dir, Path, mkAbsDir, parseAbsDir, parseRelDir, reldir, relfile, toFilePath, (</>))
import Path.IO (createDirIfMissing, removeDir, withSystemTempDir)
import ProjectTemplates.App.Cli (Options (..))
import ProjectTemplates.App.Config hiding (current)
import qualified ProjectTemplates.App.Config as Config
import ProjectTemplates.App.Errors (RunTimeError (..))
import ProjectTemplates.Templates.Config (TemplateConfig (..))
import System.FilePath (dropTrailingPathSeparator)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = do
  describe "AppConfig" $ do
    describe "buildAppConfig" $ do
      it "builds config with absolute template directory" $ do
        withSystemTempDir "config-test" $ \tmpDir -> do
          let templateName = "my-template"
          templateDir <- (tmpDir </>) <$> parseRelDir (T.unpack templateName)
          createDirIfMissing True templateDir

          let options =
                Options
                  { template = Just templateName,
                    templatesDir = toFilePath tmpDir,
                    current = False,
                    verbosity = 2,
                    listTemplates = False
                  }

          config <- buildAppConfig options
          toFilePath templateDir `shouldBe` toFilePath (Config.templateDir config)
          Config.current config `shouldBe` False

      it "handles missing template directory" $ do
        withSystemTempDir "config-test" $ \tmpDir -> do
          let options =
                Options
                  { template = Just "nonexistent",
                    templatesDir = toFilePath tmpDir,
                    current = False,
                    verbosity = 1,
                    listTemplates = False
                  }

          result <- try $ buildAppConfig options
          case result of
            Left (RunTimeError msg) -> T.unpack msg `shouldContain` "not found"
            _ -> expectationFailure "Expected RunTimeError"

      it "handles missing templates directory" $ do
        let options =
              Options
                { template = Just "any-template",
                  templatesDir = "/nonexistent/path/to/templates",
                  current = False,
                  verbosity = 0,
                  listTemplates = False
                }

        result <- try $ buildAppConfig options
        case result of
          Left (RunTimeError msg) -> T.unpack msg `shouldContain` "does not exist"
          _ -> expectationFailure "Expected RunTimeError"

      it "lists available templates when template not found" $ do
        withSystemTempDir "config-test" $ \tmpDir -> do
          -- Create some template directories
          template1 <- (tmpDir </>) <$> parseRelDir "template1"
          template2 <- (tmpDir </>) <$> parseRelDir "template2"
          createDirIfMissing True template1
          createDirIfMissing True template2

          let options =
                Options
                  { template = Just "nonexistent",
                    templatesDir = toFilePath tmpDir,
                    current = False,
                    verbosity = 2,
                    listTemplates = False
                  }

          result <- try $ buildAppConfig options
          case result of
            Left (RunTimeError msg) -> do
              T.unpack msg `shouldContain` "not found"
              T.unpack msg `shouldContain` "template1"
              T.unpack msg `shouldContain` "template2"
            _ -> expectationFailure "Expected RunTimeError with available templates"

    describe "getLogLevel" $ do
      it "maps verbosity levels correctly" $ do
        withSystemTempDir "config-test" $ \tmpDir -> do
          templateDir <- (tmpDir </>) <$> parseRelDir "test"
          createDirIfMissing True templateDir

          let testCases = [(0, "LevelError"), (1, "LevelWarn"), (2, "LevelInfo"), (3, "LevelDebug"), (4, "LevelDebug")]

          forM_ testCases $ \(v, expected) -> do
            let options =
                  Options
                    { template = Just "test",
                      templatesDir = toFilePath tmpDir,
                      current = False,
                      verbosity = v,
                      listTemplates = False
                    }
            config <- buildAppConfig options
            show (logLevel config) `shouldBe` expected

    describe "templateFilePath" $ do
      it "correctly combines template directory with relative file path" $ do
        withSystemTempDir "config-test" $ \tmpDir -> do
          templateDir <- (tmpDir </>) <$> parseRelDir "my-template"
          createDirIfMissing True templateDir

          let options =
                Options
                  { template = Just "my-template",
                    templatesDir = toFilePath tmpDir,
                    current = False,
                    verbosity = 2,
                    listTemplates = False
                  }

          config <- buildAppConfig options
          let filePath = templateFilePath [relfile|src/main.rs|] config
          toFilePath filePath `shouldContain` "src/main.rs"

    describe "templateConfigPath" $ do
      it "returns correct config.yaml path" $ do
        withSystemTempDir "config-test" $ \tmpDir -> do
          templateDir <- (tmpDir </>) <$> parseRelDir "my-template"
          createDirIfMissing True templateDir

          let options =
                Options
                  { template = Just "my-template",
                    templatesDir = toFilePath tmpDir,
                    current = False,
                    verbosity = 2,
                    listTemplates = False
                  }

          config <- buildAppConfig options
          let configPath = templateConfigPath config
          toFilePath configPath `shouldContain` "config.yaml"

  describe "listAvailableTemplates" $ do
    it "lists all template directories" $ do
      withSystemTempDir "config-test" $ \tmpDir -> do
        -- Create template directories
        template1 <- (tmpDir </>) <$> parseRelDir "rust-cli"
        template2 <- (tmpDir </>) <$> parseRelDir "haskell-lib"
        createDirIfMissing True template1
        createDirIfMissing True template2

        templates <- listAvailableTemplates (toFilePath tmpDir)
        length templates `shouldBe` 2

        let templateNames = map templateName templates
        templateNames `shouldContain` ["rust-cli"]
        templateNames `shouldContain` ["haskell-lib"]

    it "includes descriptions from config.yaml when available" $ do
      withSystemTempDir "config-test" $ \tmpDir -> do
        -- Create template with config
        templateDir <- (tmpDir </>) <$> parseRelDir "with-config"
        createDirIfMissing True templateDir

        -- Write config.yaml
        let configContent = "description: \"A template with description\"\nvariables: {}\n"
        writeFile (toFilePath $ templateDir </> [relfile|config.yaml|]) configContent

        templates <- listAvailableTemplates (toFilePath tmpDir)
        length templates `shouldBe` 1

        let template = head templates
        templateName template `shouldBe` "with-config"
        templateDescription template `shouldBe` Just "A template with description"

    it "handles templates without config.yaml" $ do
      withSystemTempDir "config-test" $ \tmpDir -> do
        -- Create template without config
        templateDir <- (tmpDir </>) <$> parseRelDir "no-config"
        createDirIfMissing True templateDir

        templates <- listAvailableTemplates (toFilePath tmpDir)
        length templates `shouldBe` 1

        let template = head templates
        templateName template `shouldBe` "no-config"
        templateDescription template `shouldBe` Nothing

    it "throws error when templates directory doesn't exist" $ do
      result <- try $ listAvailableTemplates "/nonexistent/templates"
      case result of
        Left (RunTimeError msg) -> T.unpack msg `shouldContain` "does not exist"
        _ -> expectationFailure "Expected RunTimeError"
