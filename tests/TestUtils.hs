module TestUtils where

import qualified Data.Map as M
import qualified Data.Text as T
import ProjectTemplates.Templates.Config (TemplateConfig(..), VariableConfig(..), VariableDefinitions)

-- | Create a simple variable definitions map for testing
createVariables :: [(T.Text, T.Text)] -> VariableDefinitions
createVariables = M.fromList

-- | Create a template config with variables for testing
createTemplateConfig :: [(T.Text, T.Text)] -> TemplateConfig
createTemplateConfig vars = TemplateConfig
  { variables = M.fromList $ map (\(k, v) -> (k, VariableConfig (Just v) Nothing)) vars
  , preProcess = []
  , postProcess = []
  , description = Nothing
  , overwrite = []
  }

-- | Create a template config with default values for testing
createTemplateConfigWithDefaults :: [(T.Text, T.Text)] -> [(T.Text, T.Text)] -> TemplateConfig
createTemplateConfigWithDefaults values defaults = TemplateConfig
  { variables = M.union valueVars defaultVars
  , preProcess = []
  , postProcess = []
  , description = Nothing
  , overwrite = []
  }
  where
    valueVars = M.fromList $ map (\(k, v) -> (k, VariableConfig (Just v) Nothing)) values
    defaultVars = M.fromList $ map (\(k, v) -> (k, VariableConfig Nothing (Just v))) defaults

-- | Create a simple template string for testing
simpleTemplate :: T.Text -> T.Text
simpleTemplate var = T.concat ["Hello {{ ", var, " }}, welcome!"]

-- | Create a template with multiple variables
multiVarTemplate :: [T.Text] -> T.Text
multiVarTemplate vars = T.intercalate " " $ map (\v -> T.concat ["{{ ", v, " }}"]) vars

-- | Create a template with transforms
transformTemplate :: T.Text -> T.Text -> T.Text
transformTemplate var transform = T.concat ["{{ ", var, " | ", transform, " }}"]

-- | Create a template with chained transforms
chainedTransformTemplate :: T.Text -> [T.Text] -> T.Text
chainedTransformTemplate var transforms = 
  T.concat ["{{ ", var, " | ", T.intercalate " | " transforms, " }}"]

-- | Test data for various transform scenarios
transformTestData :: [(T.Text, T.Text, T.Text)]  -- (input, transform, expected)
transformTestData = 
  [ ("hello_world", "camelcase", "helloWorld")
  , ("hello-world", "camelcase", "helloWorld")
  , ("Hello World", "camelcase", "helloWorld")
  , ("helloWorld", "snakecase", "hello_world")
  , ("hello-world", "snakecase", "hello_world")
  , ("Hello World", "snakecase", "hello_world")
  , ("hello_world", "kebabcase", "hello-world")
  , ("helloWorld", "kebabcase", "hello-world")
  , ("Hello World", "kebabcase", "hello-world")
  , ("hello_world", "pascalcase", "HelloWorld")
  , ("hello-world", "pascalcase", "HelloWorld")
  , ("Hello World", "pascalcase", "HelloWorld")
  , ("Hello World", "uppercase", "HELLO WORLD")
  , ("Hello World", "lowercase", "hello world")
  ]

-- | Test data for invalid transforms
invalidTransformData :: [T.Text]
invalidTransformData = 
  [ "unknown_transform"
  , "invalid"
  , "badtransform"
  , ""
  , "UPPERCASE"  -- should be "uppercase"
  ]

-- | Test data for transform chaining
chainTestData :: [(T.Text, [T.Text], T.Text)]  -- (input, transforms, expected)
chainTestData =
  [ ("hello_world", ["lowercase", "camelcase"], "helloWorld")
  , ("HELLO-WORLD", ["lowercase", "snakecase"], "hello_world")
  , ("mixed_Case", ["lowercase", "pascalcase"], "MixedCase")
  , ("test", ["uppercase", "lowercase"], "test")
  ]

-- | Common error messages for testing
expectedErrors :: M.Map T.Text T.Text
expectedErrors = M.fromList
  [ ("unknown_transform", "Unsupported transform: unknown_transform")
  , ("missing_var", "Variable definition not found: missing_var")
  , ("parse_error", "Could not parse template:")
  ]