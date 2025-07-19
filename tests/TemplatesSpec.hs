module TemplatesSpec where

import qualified Data.Map as M
import qualified Data.Text as T
import ProjectTemplates.Templates.Config (TemplateError (..))
import ProjectTemplates.Templates.Processor (collectVariables, fillTemplate)
import Test.Hspec

spec_collectVariables :: Spec
spec_collectVariables = do
  describe "collectVariables" $ do
    it "should collect variables from a simple bracket" $ do
      let template = "{{ key }}"
      collectVariables template `shouldBe` Right ["key"]
    it "should collect variables from a template" $ do
      let template = "{{ key }}: {{ value }},"
      collectVariables template `shouldBe` Right ["key", "value"]
    it "should collect nothing from an invalid bracket" $ do
      let template = "{{ key/// }} : {{ value }},"
      collectVariables template `shouldBe` Right ["value"]

spec_fillTemplate :: Spec
spec_fillTemplate = do
  describe "fillTemplate" $ do
    it "should fill a template with variables" $ do
      let template = "{{ key }}: {{ value }},"
      let variables = M.fromList [("key", "someKey"), ("value", "100")]
      fillTemplate variables template `shouldBe` Right "someKey: 100,"

    it "should throw an error when a variable was not found" $ do
      let template = "{{ key }}: {{ value }},"
      let variables = M.fromList [("key", "someKey")]
      fillTemplate variables template `shouldBe` Left (DefinitionNotFoundError "value")

    it "should ignore malformed values" $ do
      let template = "{{ key/// }}: {{ value }},"
      let variables = M.fromList [("key", "someKey"), ("value", "100")]
      fillTemplate variables template `shouldBe` Right "{{ key/// }}: 100,"

    it "should apply transforms correctly" $ do
      let template = "{{ key | camelcase }}: {{ value | uppercase }},"
      let variables = M.fromList [("key", "some_key"), ("value", "abc")]
      fillTemplate variables template `shouldBe` Right "someKey: ABC,"

    it "should throw an error if a transform is not known" $ do
      let template = "{{ key | camelcase }}: {{ value | unknown_transform }},"
      let variables = M.fromList [("key", "some_key"), ("value", "abc")]
      fillTemplate variables template `shouldBe` Left (UnsupportedTransform "unknown_transform")

    it "should ignore malformed transforms" $ do
      let template = "{{ key | camelcase/// }}: {{ value }},"
      let variables = M.fromList [("key", "some_key"), ("value", "abc")]
      fillTemplate variables template `shouldBe` Right "{{ key | camelcase/// }}: abc,"

    -- Edge Cases
    describe "edge cases" $ do
      it "handles empty templates" $ do
        let template = ""
        let variables = M.empty
        fillTemplate variables template `shouldBe` Right ""
        collectVariables template `shouldBe` Right []

      it "handles templates with no variables" $ do
        let template = "This is just plain text with no variables"
        let variables = M.empty
        fillTemplate variables template `shouldBe` Right "This is just plain text with no variables"
        collectVariables template `shouldBe` Right []

      it "handles templates with only whitespace" $ do
        let template = "   \n\t  \r\n  "
        let variables = M.empty
        fillTemplate variables template `shouldBe` Right "   \n\t  \r\n  "

      it "handles nested brackets correctly" $ do
        let template = "{{{{ nested }}}}"
        let variables = M.fromList [("nested", "value")]
        -- Parser should handle this gracefully, leaving outer brackets as text
        fillTemplate variables template `shouldBe` Right "{{{{ nested }}}}"

      it "handles multiple consecutive variables" $ do
        let template = "{{a}}{{b}}{{c}}"
        let variables = M.fromList [("a", "1"), ("b", "2"), ("c", "3")]
        fillTemplate variables template `shouldBe` Right "123"

      it "handles variables with special characters in names" $ do
        let template = "{{var-name}} and {{var_name}}"
        let variables = M.fromList [("var-name", "dash"), ("var_name", "underscore")]
        fillTemplate variables template `shouldBe` Right "dash and underscore"

      it "handles very long variable names" $ do
        let longVarName = T.replicate 100 "a"
        let template = "{{" <> longVarName <> "}}"
        let variables = M.fromList [(longVarName, "value")]
        fillTemplate variables template `shouldBe` Right "value"

      it "handles very long variable values" $ do
        let longValue = T.replicate 1000 "x"
        let template = "{{var}}"
        let variables = M.fromList [("var", longValue)]
        fillTemplate variables template `shouldBe` Right longValue

      it "handles empty variable values" $ do
        let template = "start{{empty}}end"
        let variables = M.fromList [("empty", "")]
        fillTemplate variables template `shouldBe` Right "startend"

      it "handles unicode characters in templates" $ do
        let template = "Hello {{name}} 🚀 {{emoji}}"
        let variables = M.fromList [("name", "世界"), ("emoji", "🎉")]
        fillTemplate variables template `shouldBe` Right "Hello 世界 🚀 🎉"

      it "handles single character variables" $ do
        let template = "{{a}} {{b}} {{c}}"
        let variables = M.fromList [("a", "x"), ("b", "y"), ("c", "z")]
        fillTemplate variables template `shouldBe` Right "x y z"

    describe "malformed template handling" $ do
      it "handles unclosed brackets at end" $ do
        let template = "start {{unclosed"
        let variables = M.empty
        fillTemplate variables template `shouldBe` Right "start {{unclosed"

      it "handles unopened brackets" $ do
        let template = "start }} end"
        let variables = M.empty
        fillTemplate variables template `shouldBe` Right "start }} end"

      it "handles mismatched brackets" $ do
        let template = "{{ valid }} { invalid } {{{{ also_invalid"
        let variables = M.fromList [("valid", "OK")]
        fillTemplate variables template `shouldBe` Right "OK { invalid } {{{{ also_invalid"

      it "handles empty brackets" $ do
        let template = "start {{}} end"
        let variables = M.empty
        fillTemplate variables template `shouldBe` Right "start {{}} end"

      it "handles brackets with only whitespace" $ do
        let template = "start {{   }} end"
        let variables = M.empty
        -- Empty variable name should be ignored
        fillTemplate variables template `shouldBe` Right "start {{   }} end"

      it "handles mixed valid and invalid syntax" $ do
        let template = "{{valid}} and {{invalid syntax here}} and {{another}}"
        let variables = M.fromList [("valid", "OK"), ("another", "GOOD")]
        fillTemplate variables template `shouldBe` Right "OK and {{invalid syntax here}} and GOOD"

    describe "large template performance" $ do
      it "handles templates with many variables efficiently" $ do
        -- Create a template with 100 variables
        let varCount = 100
        let templateParts = map (\i -> "{{var" <> T.pack (show i) <> "}}") [1..varCount]
        let template = T.intercalate " " templateParts
        let variables = M.fromList $ map (\i -> ("var" <> T.pack (show i), "value" <> T.pack (show i))) [1..varCount]
        
        case fillTemplate variables template of
          Right result -> T.length result `shouldSatisfy` (> 0)
          Left err -> expectationFailure $ show err

      it "handles very large templates" $ do
        -- Create a large template (10KB)
        let baseTemplate = "This is {{var}} repeated many times. "
        let largeTemplate = T.replicate 300 baseTemplate  -- ~10KB
        let variables = M.fromList [("var", "EXPANDED_VALUE")]  -- Longer replacement
        
        case fillTemplate variables largeTemplate of
          Right result -> do
            T.length result `shouldSatisfy` (> 0)  -- Just ensure it processes successfully
            T.unpack result `shouldContain` "EXPANDED_VALUE"  -- And contains our replacement
          Left err -> expectationFailure $ show err

    describe "variable name validation" $ do
      it "accepts alphanumeric variable names" $ do
        let template = "{{abc123}} {{xyz789}}"
        let variables = M.fromList [("abc123", "val1"), ("xyz789", "val2")]
        fillTemplate variables template `shouldBe` Right "val1 val2"

      it "accepts variables with hyphens and underscores" $ do
        let template = "{{var-name}} {{var_name}} {{var-name_123}}"
        let variables = M.fromList [("var-name", "dash"), ("var_name", "underscore"), ("var-name_123", "mixed")]
        fillTemplate variables template `shouldBe` Right "dash underscore mixed"

      it "ignores variables with invalid characters" $ do
        let template = "{{valid}} {{in@valid}} {{al$o-invalid}}"
        let variables = M.fromList [("valid", "OK")]
        -- Invalid variable names should be left as-is
        fillTemplate variables template `shouldBe` Right "OK {{in@valid}} {{al$o-invalid}}"

    describe "whitespace handling" $ do
      it "handles variables with surrounding whitespace" $ do
        let template = "{{ var }} {{  spaced  }} {{\ttabbed\t}}"
        let variables = M.fromList [("var", "value1"), ("spaced", "value2"), ("tabbed", "value3")]
        fillTemplate variables template `shouldBe` Right "value1 value2 value3"

      it "preserves whitespace in template content" $ do
        let template = "start   {{var}}   middle   {{var2}}   end"
        let variables = M.fromList [("var", "A"), ("var2", "B")]
        fillTemplate variables template `shouldBe` Right "start   A   middle   B   end"
