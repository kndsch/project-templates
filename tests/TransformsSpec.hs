module TransformsSpec where

import qualified Data.Text as T
import ProjectTemplates.Templates.Config (TemplateError (..))
import ProjectTemplates.Templates.Processor (fillTemplate)
import ProjectTemplates.Templates.Transforms (applyTransform)
import Test.Hspec
import TestUtils

spec_applyTransform :: Spec
spec_applyTransform = do
  describe "applyTransform" $ do
    describe "individual transforms" $ do
      it "should apply camelcase transform correctly" $ do
        applyTransform "hello_world" "camelcase" `shouldBe` Right "helloWorld"
        applyTransform "hello-world" "camelcase" `shouldBe` Right "helloWorld"
        applyTransform "Hello World" "camelcase" `shouldBe` Right "helloWorld"
        applyTransform "HELLO_WORLD" "camelcase" `shouldBe` Right "hellOWorld"

      it "should apply snakecase transform correctly" $ do
        applyTransform "helloWorld" "snakecase" `shouldBe` Right "hello_World"
        applyTransform "hello-world" "snakecase" `shouldBe` Right "hello_world"
        applyTransform "Hello World" "snakecase" `shouldBe` Right "Hello_World"
        applyTransform "HELLO_WORLD" "snakecase" `shouldBe` Right "HELL_O_WORLD"

      it "should apply kebabcase transform correctly" $ do
        applyTransform "hello_world" "kebabcase" `shouldBe` Right "hello-world"
        applyTransform "helloWorld" "kebabcase" `shouldBe` Right "hello-world"
        applyTransform "Hello World" "kebabcase" `shouldBe` Right "hello-world"
        applyTransform "HELLO_WORLD" "kebabcase" `shouldBe` Right "hell-o-world"

      it "should apply pascalcase transform correctly" $ do
        applyTransform "hello_world" "pascalcase" `shouldBe` Right "HelloWorld"
        applyTransform "hello-world" "pascalcase" `shouldBe` Right "HelloWorld"
        applyTransform "Hello World" "pascalcase" `shouldBe` Right "HelloWorld"
        applyTransform "helloWorld" "pascalcase" `shouldBe` Right "HelloWorld"

      it "should apply uppercase transform correctly" $ do
        applyTransform "hello world" "uppercase" `shouldBe` Right "HELLO WORLD"
        applyTransform "Hello World" "uppercase" `shouldBe` Right "HELLO WORLD"
        applyTransform "HELLO WORLD" "uppercase" `shouldBe` Right "HELLO WORLD"

      it "should apply lowercase transform correctly" $ do
        applyTransform "HELLO WORLD" "lowercase" `shouldBe` Right "hello world"
        applyTransform "Hello World" "lowercase" `shouldBe` Right "hello world"
        applyTransform "hello world" "lowercase" `shouldBe` Right "hello world"

    describe "transform error cases" $ do
      it "should return error for unknown transform" $ do
        applyTransform "test" "unknown_transform" `shouldBe` Left (UnsupportedTransform "unknown_transform")
        applyTransform "test" "invalid" `shouldBe` Left (UnsupportedTransform "invalid")
        applyTransform "test" "" `shouldBe` Left (UnsupportedTransform "")

      it "should handle case sensitivity in transform names" $ do
        applyTransform "test" "CAMELCASE" `shouldBe` Right "test"
        applyTransform "test" "CamelCase" `shouldBe` Right "test"

    describe "edge cases" $ do
      it "should handle empty input strings" $ do
        applyTransform "" "camelcase" `shouldBe` Right ""
        applyTransform "" "uppercase" `shouldBe` Right ""
        applyTransform "" "lowercase" `shouldBe` Right ""

      it "should handle single character inputs" $ do
        applyTransform "a" "uppercase" `shouldBe` Right "A"
        applyTransform "A" "lowercase" `shouldBe` Right "a"
        applyTransform "a" "camelcase" `shouldBe` Right "a"

      it "should handle special characters" $ do
        applyTransform "hello@world" "uppercase" `shouldBe` Right "HELLO@WORLD"
        applyTransform "test_123" "camelcase" `shouldBe` Right "test123"

      it "should handle unicode characters" $ do
        applyTransform "héllo_wörld" "uppercase" `shouldBe` Right "HÉLLO_WÖRLD"
        applyTransform "HÉLLO_WÖRLD" "lowercase" `shouldBe` Right "héllo_wörld"

spec_transformsInTemplates :: Spec
spec_transformsInTemplates = do
  describe "transforms in template processing" $ do
    describe "single transforms" $ do
      it "should apply transforms in template substitution" $ do
        let template = "{{ name | camelcase }}"
        let variables = createVariables [("name", "hello_world")]
        fillTemplate variables template `shouldBe` Right "helloWorld"

      it "should apply multiple different transforms" $ do
        let template = "{{ name | uppercase }} and {{ other | lowercase }}"
        let variables = createVariables [("name", "hello"), ("other", "WORLD")]
        fillTemplate variables template `shouldBe` Right "HELLO and world"

    describe "transform chaining" $ do
      it "should chain transforms correctly" $ do
        let template = "{{name|lowercase|camelcase}}"
        let variables = createVariables [("name", "HELLO_WORLD")]
        fillTemplate variables template `shouldBe` Right "helloWorld"

      it "should handle multiple chained transforms" $ do
        let template = "{{name|lowercase|snakecase|uppercase}}"
        let variables = createVariables [("name", "HelloWorld")]
        fillTemplate variables template `shouldBe` Right "HELLOWORLD"

      it "should apply transforms in correct order" $ do
        let template = "{{name|uppercase|lowercase}}"
        let variables = createVariables [("name", "Hello")]
        fillTemplate variables template `shouldBe` Right "hello"

    describe "transform error handling in templates" $ do
      it "should propagate unknown transform errors" $ do
        let template = "{{ name | unknown_transform }}"
        let variables = createVariables [("name", "test")]
        fillTemplate variables template `shouldBe` Left (UnsupportedTransform "unknown_transform")

      it "should handle error in transform chain" $ do
        let template = "{{name|lowercase|unknown_transform}}"
        let variables = createVariables [("name", "test")]
        fillTemplate variables template `shouldBe` Left (UnsupportedTransform "unknown_transform")

      it "should handle multiple variables with some having invalid transforms" $ do
        let template = "{{ good | uppercase }} {{ bad | invalid_transform }}"
        let variables = createVariables [("good", "test"), ("bad", "test")]
        fillTemplate variables template `shouldBe` Left (UnsupportedTransform "invalid_transform")

spec_transformPerformance :: Spec
spec_transformPerformance = do
  describe "transform performance" $ do
    it "should handle large input strings efficiently" $ do
      let largeInput = T.replicate 1000 "hello_world_"
      let result = applyTransform largeInput "camelcase"
      result `shouldSatisfy` (\x -> case x of
        Right _ -> True
        Left _ -> False)

    it "should handle many chained transforms" $ do
      let template = "{{name|lowercase|uppercase|lowercase|camelcase}}"
      let variables = createVariables [("name", "test_input")]
      fillTemplate variables template `shouldBe` Right "testInput"

    it "should handle templates with many variables and transforms" $ do
      let vars = map (\i -> ("var" <> T.pack (show i), "test_value")) [1..50]
      let templateParts = map (\i -> "{{var" <> T.pack (show i) <> "|camelcase}}") [1..50]
      let template = T.intercalate " " templateParts
      let variables = createVariables vars
      let result = fillTemplate variables template
      result `shouldSatisfy` (\x -> case x of
        Right _ -> True
        Left _ -> False)