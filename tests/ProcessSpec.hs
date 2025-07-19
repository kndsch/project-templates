{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ProcessSpec (spec) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import ProjectTemplates.Templates.Config (TemplateError(..), VariableDefinitions)
import ProjectTemplates.Templates.Processor (fillTemplate)
import Test.Hspec

spec :: Spec
spec = do
  describe "File Processing" $ do
    describe "fillTemplate processor" $ do
      it "processes template content with variable substitution" $ do
          let vars = M.fromList [("name", "Alice"), ("project", "MyApp")]
          let template = "Hello {{name}}, welcome to {{project}}!"
          
          case fillTemplate vars template of
            Right processed -> processed `shouldBe` "Hello Alice, welcome to MyApp!"
            Left err -> expectationFailure $ show err
            
      it "handles files with multiple variables and transforms" $ do
          let vars = M.fromList [("project", "my-cool-app"), ("author", "john_doe")]
          let template = "{{project|uppercase}} by {{author|camelcase}}"
          
          case fillTemplate vars template of
            Right processed -> processed `shouldBe` "MY-COOL-APP by johnDoe"
            Left err -> expectationFailure $ show err
            
      it "processes file names with template variables" $ do
          let vars = M.fromList [("module", "user"), ("ext", "rs")]
          let fileName = "{{module}}_service.{{ext}}"
          
          case fillTemplate vars fileName of
            Right processed -> processed `shouldBe` "user_service.rs"
            Left err -> expectationFailure $ show err
            
      it "handles nested paths with variables" $ do
          let vars = M.fromList [("module", "auth"), ("component", "login")]
          let fileName = "src/{{module}}/{{component}}.tsx"
          
          case fillTemplate vars fileName of
            Right processed -> processed `shouldBe` "src/auth/login.tsx"
            Left err -> expectationFailure $ show err
            
    describe "template error handling" $ do
      it "handles missing variables" $ do
          let vars = M.fromList []  -- Missing variable
          let template = "Hello {{missing}}"
          
          case fillTemplate vars template of
            Right _ -> expectationFailure "Expected an error for missing variable"
            Left (DefinitionNotFoundError var) -> var `shouldBe` "missing"
            Left err -> expectationFailure $ "Unexpected error: " ++ show err
            
      it "handles malformed templates" $ do
          let vars = M.fromList [("name", "test")]
          let template = "Hello {{unclosed"
          
          case fillTemplate vars template of
            Right processed -> processed `shouldBe` "Hello {{unclosed"  -- Unclosed brackets are left as-is
            Left err -> expectationFailure $ "Unexpected error: " ++ show err
            
      it "handles unknown transforms" $ do
          let vars = M.fromList [("name", "test")]
          let template = "Hello {{name|unknowntransform}}"
          
          case fillTemplate vars template of
            Right _ -> expectationFailure "Expected an error for unknown transform"
            Left (UnsupportedTransform transform) -> transform `shouldBe` "unknowntransform"
            Left err -> expectationFailure $ "Unexpected error: " ++ show err
            
    describe "complex template scenarios" $ do
      it "handles multiple variables in paths" $ do
          let vars = M.fromList 
                [ ("org", "mycompany")
                , ("project", "webapp")
                , ("module", "auth")
                , ("version", "v2")
                ]
          let template = "{{org}}/{{project}}/src/{{module}}/{{version}}/index.ts"
          
          case fillTemplate vars template of
            Right processed -> processed `shouldBe` "mycompany/webapp/src/auth/v2/index.ts"
            Left err -> expectationFailure $ show err
            
      it "handles empty variable values" $ do
          let vars = M.fromList [("prefix", ""), ("name", "file"), ("suffix", "")]
          let template = "{{prefix}}{{name}}{{suffix}}.txt"
          
          case fillTemplate vars template of
            Right processed -> processed `shouldBe` "file.txt"
            Left err -> expectationFailure $ show err
            
      it "preserves non-template content exactly" $ do
          let vars = M.fromList [("var", "VALUE"), ("that", "THAT")]
          let template = "Keep { this } and {{ that }} but replace {{var}}"
          
          case fillTemplate vars template of
            Right processed -> processed `shouldBe` "Keep { this } and THAT but replace VALUE"
            Left err -> expectationFailure $ show err