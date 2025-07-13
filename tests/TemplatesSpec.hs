module TemplatesSpec where

import qualified Data.Map as M
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
