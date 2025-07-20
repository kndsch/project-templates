{-# LANGUAGE OverloadedStrings #-}

module HooksSpec (spec) where

import qualified Data.Map as M
import qualified Data.Text as T
import ProjectTemplates.Templates.Config (TemplateConfig (..), defaultTemplateConfig)
import ProjectTemplates.Templates.Processor (fillTemplate)
import Test.Hspec

spec :: Spec
spec = do
  describe "Hook System" $ do
    describe "Template Processing in Hooks" $ do
      it "processes variables in hook commands" $ do
        let variables = M.fromList [("name", "myproject"), ("version", "1.0.0")]
        let hookTemplate = "echo 'Building {{name}} version {{version}}'"
        case fillTemplate variables hookTemplate of
          Right processed -> processed `shouldBe` "echo 'Building myproject version 1.0.0'"
          Left err -> expectationFailure $ show err

      it "handles multiple variables in single hook command" $ do
        let variables = M.fromList [("project", "webapp"), ("author", "john"), ("license", "MIT")]
        let hookTemplate = "git config --local user.name '{{author}}' && echo 'Created {{project}} with {{license}} license'"
        case fillTemplate variables hookTemplate of
          Right processed -> processed `shouldBe` "git config --local user.name 'john' && echo 'Created webapp with MIT license'"
          Left err -> expectationFailure $ show err

      it "handles hook commands with no variables" $ do
        let variables = M.empty
        let hookTemplate = "echo 'No variables here'"
        case fillTemplate variables hookTemplate of
          Right processed -> processed `shouldBe` "echo 'No variables here'"
          Left err -> expectationFailure $ show err

      it "fails when hook template has undefined variables" $ do
        let variables = M.fromList [("defined", "value")]
        let hookTemplate = "echo '{{defined}} and {{undefined}}'"
        case fillTemplate variables hookTemplate of
          Right _ -> expectationFailure "Should have failed with undefined variable"
          Left err -> show err `shouldContain` "undefined"

      it "processes transforms in hook commands" $ do
        let variables = M.fromList [("name", "my_project"), ("type", "library")]
        let hookTemplate = "mkdir {{name|kebabcase}}-{{type|uppercase}}"
        case fillTemplate variables hookTemplate of
          Right processed -> processed `shouldBe` "mkdir my-project-LIBRARY"
          Left err -> expectationFailure $ show err

    describe "Template Configuration Hooks" $ do
      it "extracts pre-process hooks from config" $ do
        let config = TemplateConfig M.empty ["echo 'pre1'", "echo 'pre2'"] [] Nothing []
        preProcess config `shouldBe` ["echo 'pre1'", "echo 'pre2'"]

      it "extracts post-process hooks from config" $ do
        let config = TemplateConfig M.empty [] ["echo 'post1'", "echo 'post2'"] Nothing []
        postProcess config `shouldBe` ["echo 'post1'", "echo 'post2'"]

      it "handles empty hook lists" $ do
        let config = defaultTemplateConfig
        preProcess config `shouldBe` []
        postProcess config `shouldBe` []

      it "handles configs with mixed hooks" $ do
        let config = TemplateConfig M.empty ["pre-hook"] ["post-hook"] Nothing []
        preProcess config `shouldBe` ["pre-hook"]
        postProcess config `shouldBe` ["post-hook"]

    describe "Hook Command Composition" $ do
      it "handles complex shell commands with variables" $ do
        let variables = M.fromList [("project", "myapp"), ("dir", "/tmp/build")]
        let hookTemplate = "cd {{dir}} && make clean && make {{project}}"
        case fillTemplate variables hookTemplate of
          Right processed -> processed `shouldBe` "cd /tmp/build && make clean && make myapp"
          Left err -> expectationFailure $ show err

      it "handles quoted variables in commands" $ do
        let variables = M.fromList [("name", "project with spaces"), ("path", "/path/with spaces")]
        let hookTemplate = "echo \"Project: {{name}}\" && mkdir \"{{path}}\""
        case fillTemplate variables hookTemplate of
          Right processed -> processed `shouldBe` "echo \"Project: project with spaces\" && mkdir \"/path/with spaces\""
          Left err -> expectationFailure $ show err

      it "handles script execution with variables" $ do
        let variables = M.fromList [("script", "setup.sh"), ("env", "production")]
        let hookTemplate = "chmod +x {{script}} && ./{{script}} --env {{env}}"
        case fillTemplate variables hookTemplate of
          Right processed -> processed `shouldBe` "chmod +x setup.sh && ./setup.sh --env production"
          Left err -> expectationFailure $ show err

    describe "Hook Error Scenarios" $ do
      it "identifies missing variables in hook templates" $ do
        let variables = M.fromList [("project", "myapp")]
        let hookTemplate = "echo {{project}} && echo {{missing_var}}"
        case fillTemplate variables hookTemplate of
          Right _ -> expectationFailure "Should fail with missing variable"
          Left err -> show err `shouldContain` "missing_var"

      it "identifies unsupported transforms in hooks" $ do
        let variables = M.fromList [("name", "project")]
        let hookTemplate = "echo {{name|unknown_transform}}"
        case fillTemplate variables hookTemplate of
          Right _ -> expectationFailure "Should fail with unknown transform"
          Left err -> show err `shouldContain` "unknown_transform"

      it "handles malformed variable syntax in hooks" $ do
        let variables = M.fromList [("valid", "value")]
        let hookTemplate = "echo {{valid}} && echo {{invalid syntax}}"
        case fillTemplate variables hookTemplate of
          Right processed -> T.unpack processed `shouldContain` "value"
          Left err -> expectationFailure $ show err

    describe "Directory Context for Hooks" $ do
      -- These tests document the expected behavior of directory changes
      -- The actual implementation changes working directory for hook execution
      
      it "pre-process hooks should use working directory context" $ do
        -- Pre-process hooks run in the current working directory
        let hookTemplate = "pwd && ls -la"
        let variables = M.empty
        case fillTemplate variables hookTemplate of
          Right processed -> processed `shouldBe` "pwd && ls -la"
          Left err -> expectationFailure $ show err

      it "post-process hooks should use target directory context" $ do
        -- Post-process hooks run in the target directory after processing
        let variables = M.fromList [("target", "/output/dir")]
        let hookTemplate = "cd {{target}} && pwd"
        case fillTemplate variables hookTemplate of
          Right processed -> processed `shouldBe` "cd /output/dir && pwd"
          Left err -> expectationFailure $ show err

    describe "Real-world Hook Examples" $ do
      it "handles git initialization hook" $ do
        let variables = M.fromList [("project", "myproject"), ("author", "John Doe")]
        let hookTemplate = "git init && git config user.name '{{author}}' && git add . && git commit -m 'Initial commit for {{project}}'"
        case fillTemplate variables hookTemplate of
          Right processed -> do
            T.unpack processed `shouldContain` "git init"
            T.unpack processed `shouldContain` "John Doe"
            T.unpack processed `shouldContain` "myproject"
          Left err -> expectationFailure $ show err

      it "handles npm package initialization" $ do
        let variables = M.fromList [("name", "my-package"), ("version", "0.1.0"), ("description", "A cool package")]
        let hookTemplate = "npm init -y && npm pkg set name={{name}} version={{version}} description='{{description}}'"
        case fillTemplate variables hookTemplate of
          Right processed -> do
            T.unpack processed `shouldContain` "npm init -y"
            T.unpack processed `shouldContain` "name=my-package"
            T.unpack processed `shouldContain` "version=0.1.0"
            T.unpack processed `shouldContain` "description='A cool package'"
          Left err -> expectationFailure $ show err

      it "handles docker build hook" $ do
        let variables = M.fromList [("image", "myapp"), ("tag", "latest"), ("dockerfile", "Dockerfile.prod")]
        let hookTemplate = "docker build -f {{dockerfile}} -t {{image}}:{{tag}} ."
        case fillTemplate variables hookTemplate of
          Right processed -> processed `shouldBe` "docker build -f Dockerfile.prod -t myapp:latest ."
          Left err -> expectationFailure $ show err

      it "handles dependency installation hook" $ do
        let variables = M.fromList [("package_manager", "cabal"), ("project", "haskell-app")]
        let hookTemplate = "{{package_manager}} update && {{package_manager}} install --dependencies-only && echo 'Dependencies installed for {{project}}'"
        case fillTemplate variables hookTemplate of
          Right processed -> processed `shouldBe` "cabal update && cabal install --dependencies-only && echo 'Dependencies installed for haskell-app'"
          Left err -> expectationFailure $ show err

    describe "Hook Security Considerations" $ do
      it "handles variables with special shell characters" $ do
        let variables = M.fromList [("name", "project; rm -rf /"), ("cmd", "ls && rm file")]
        let hookTemplate = "echo 'Name: {{name}}' && echo 'Command: {{cmd}}'"
        case fillTemplate variables hookTemplate of
          Right processed -> do
            -- Variables are substituted as-is, shell escaping is user responsibility
            T.unpack processed `shouldContain` "project; rm -rf /"
            T.unpack processed `shouldContain` "ls && rm file"
          Left err -> expectationFailure $ show err

      it "handles empty variable values in hooks" $ do
        let variables = M.fromList [("empty", ""), ("normal", "value")]
        let hookTemplate = "echo '{{empty}}' && echo '{{normal}}'"
        case fillTemplate variables hookTemplate of
          Right processed -> processed `shouldBe` "echo '' && echo 'value'"
          Left err -> expectationFailure $ show err

    describe "Performance with Many Hooks" $ do
      it "handles multiple hook commands efficiently" $ do
        let variables = M.fromList $ map (\i -> ("var" <> T.pack (show i), "value" <> T.pack (show i))) [1..20]
        let hookCommands = map (\i -> "echo {{var" <> T.pack (show i) <> "}}") [1..20]
        let results = map (fillTemplate variables) hookCommands
        let isRight (Right _) = True
            isRight (Left _) = False
        
        -- All should succeed
        all isRight results `shouldBe` True
        
        -- Verify a few specific results
        case fillTemplate variables "echo {{var1}} && echo {{var20}}" of
          Right processed -> processed `shouldBe` "echo value1 && echo value20"
          Left err -> expectationFailure $ show err

      it "handles long hook command efficiently" $ do
        let variables = M.fromList [("repeat", "test")]
        let longHook = T.intercalate " && " $ replicate 50 "echo {{repeat}}"
        case fillTemplate variables longHook of
          Right processed -> T.length processed `shouldSatisfy` (> 0)
          Left err -> expectationFailure $ show err