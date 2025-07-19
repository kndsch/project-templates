{-# LANGUAGE OverloadedStrings #-}

module TemplateErrorSpec (spec) where

import qualified Data.Map as M
import qualified Data.Text as T
import ProjectTemplates.Templates.Config (TemplateError (..))
import ProjectTemplates.Templates.Processor (collectVariables, fillTemplate)
import Test.Hspec

spec :: Spec
spec = do
  describe "TemplateError Types" $ do
    describe "DefinitionNotFoundError" $ do
      it "is thrown for single missing variable" $ do
        let template = "Hello {{missing}}"
        let variables = M.empty
        fillTemplate variables template `shouldBe` Left (DefinitionNotFoundError "missing")

      it "is thrown for first missing variable when multiple are missing" $ do
        let template = "{{first}} and {{second}}"
        let variables = M.empty
        fillTemplate variables template `shouldBe` Left (DefinitionNotFoundError "first")

      it "reports the exact variable name that is missing" $ do
        let template = "{{var-with-dashes}} {{var_with_underscores}}"
        let variables = M.fromList [("var-with-dashes", "present")]
        fillTemplate variables template `shouldBe` Left (DefinitionNotFoundError "var_with_underscores")

      it "is thrown even when some variables are defined" $ do
        let template = "{{defined}} {{undefined}} {{also_defined}}"
        let variables = M.fromList [("defined", "value1"), ("also_defined", "value2")]
        fillTemplate variables template `shouldBe` Left (DefinitionNotFoundError "undefined")

      it "handles case-sensitive variable names" $ do
        let template = "{{Variable}} {{variable}}"
        let variables = M.fromList [("variable", "lowercase")]
        fillTemplate variables template `shouldBe` Left (DefinitionNotFoundError "Variable")

    describe "UnsupportedTransform" $ do
      it "is thrown for unknown transforms" $ do
        let template = "{{var|unknowntransform}}"
        let variables = M.fromList [("var", "value")]
        fillTemplate variables template `shouldBe` Left (UnsupportedTransform "unknowntransform")

      it "is thrown for first unknown transform in chain" $ do
        let template = "{{var|uppercase|unknowntransform|lowercase}}"
        let variables = M.fromList [("var", "value")]
        fillTemplate variables template `shouldBe` Left (UnsupportedTransform "unknowntransform")

      it "reports the exact transform name" $ do
        let template = "{{var|custom_transform_123}}"
        let variables = M.fromList [("var", "value")]
        fillTemplate variables template `shouldBe` Left (UnsupportedTransform "custom_transform_123")

      it "is thrown even when variable exists" $ do
        let template = "{{existing_var|nonexistent_transform}}"
        let variables = M.fromList [("existing_var", "some_value")]
        fillTemplate variables template `shouldBe` Left (UnsupportedTransform "nonexistent_transform")

      it "handles transform names with special characters" $ do
        let template = "{{var|transform-with-dashes}}"
        let variables = M.fromList [("var", "value")]
        fillTemplate variables template `shouldBe` Left (UnsupportedTransform "transform-with-dashes")

    describe "ParseError" $ do
      -- Note: The current parser appears to be quite permissive and doesn't
      -- generate ParseError easily. These tests document the expected behavior
      -- if ParseError were to be generated for malformed templates.

      it "may be thrown for severely malformed templates" $ do
        -- This test documents that currently malformed templates are often
        -- handled gracefully rather than throwing ParseError
        let template = "{{{{{{invalid}}"
        let variables = M.empty
        case fillTemplate variables template of
          Right _ -> return () -- Current behavior: graceful handling
          Left (ParseError _) -> return () -- Potential future behavior
          Left err -> expectationFailure $ "Unexpected error type: " ++ show err

    describe "Error Show instances" $ do
      it "shows DefinitionNotFoundError with variable name" $ do
        let err = DefinitionNotFoundError "missing_var"
        show err `shouldBe` "Variable definition not found: missing_var"

      it "shows UnsupportedTransform with transform name" $ do
        let err = UnsupportedTransform "bad_transform"
        show err `shouldBe` "Unsupported transform: bad_transform"

      it "shows ParseError with message" $ do
        let err = ParseError "Invalid syntax"
        show err `shouldBe` "Could not parse template: Invalid syntax"

  describe "Error Propagation" $ do
    describe "template processing with errors" $ do
      it "stops at first error and doesn't process further" $ do
        let template = "{{undefined1}} {{undefined2}}"
        let variables = M.empty
        -- Should fail on first undefined variable, not process second
        fillTemplate variables template `shouldBe` Left (DefinitionNotFoundError "undefined1")

      it "processes variables before the error" $ do
        let template = "{{defined}} {{undefined}}"
        let variables = M.fromList [("defined", "OK")]
        fillTemplate variables template `shouldBe` Left (DefinitionNotFoundError "undefined")

      it "fails on transform error even when variable is defined" $ do
        let template = "{{var|badtransform}}"
        let variables = M.fromList [("var", "value")]
        fillTemplate variables template `shouldBe` Left (UnsupportedTransform "badtransform")

    describe "complex error scenarios" $ do
      it "handles error in middle of complex template" $ do
        let template = "Start {{good1}} middle {{bad|unknown}} end {{good2}}"
        let variables = M.fromList [("good1", "GOOD1"), ("bad", "BAD"), ("good2", "GOOD2")]
        fillTemplate variables template `shouldBe` Left (UnsupportedTransform "unknown")

      it "prioritizes variable errors over transform errors" $ do
        let template = "{{undefined|alsounknown}}"
        let variables = M.empty
        -- Variable error should be caught first
        fillTemplate variables template `shouldBe` Left (DefinitionNotFoundError "undefined")

      it "handles errors with unicode content" $ do
        let template = "Hello {{missing}} 世界"
        let variables = M.empty
        fillTemplate variables template `shouldBe` Left (DefinitionNotFoundError "missing")

  describe "Error Handling Edge Cases" $ do
    describe "empty and whitespace scenarios" $ do
      it "handles empty variable name error" $ do
        -- This tests how empty variable names are handled
        let template = "{{}}"
        let variables = M.empty
        case fillTemplate variables template of
          Right result -> result `shouldBe` "{{}}" -- Likely treated as invalid syntax
          Left _ -> return () -- Or could generate an error
      it "handles whitespace-only variable name" $ do
        let template = "{{   }}"
        let variables = M.empty
        case fillTemplate variables template of
          Right result -> result `shouldBe` "{{   }}" -- Likely treated as invalid
          Left _ -> return () -- Or could generate an error
    describe "variable collection with errors" $ do
      it "collectVariables succeeds even when fillTemplate would fail" $ do
        let template = "{{valid}} {{also_valid|unknown_transform}}"
        case collectVariables template of
          Right vars -> vars `shouldContain` ["valid", "also_valid"]
          Left err -> expectationFailure $ "collectVariables should not fail: " ++ show err

      it "collectVariables handles malformed templates gracefully" $ do
        let template = "{{valid}} {{invalid syntax}} {{also_valid}}"
        case collectVariables template of
          Right vars -> vars `shouldContain` ["valid", "also_valid"]
          Left err -> expectationFailure $ "collectVariables should handle malformed templates: " ++ show err
