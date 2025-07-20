{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module ProjectTemplates.Templates.Config where

import Data.Aeson (FromJSON (parseJSON), withObject, (.!=), (.:?))
import qualified Data.Map as M
import qualified Data.Text as T
import GHC.Generics (Generic)

data VariableConfig = VariableConfig
  {value :: Maybe T.Text, defaultValue :: Maybe T.Text}
  deriving (Show, Generic)

data TemplateConfig = TemplateConfig
  { variables :: M.Map T.Text VariableConfig,
    preProcess :: [T.Text],
    postProcess :: [T.Text],
    description :: Maybe T.Text,
    overwrite :: [T.Text]
  }
  deriving (Show, Generic)

instance FromJSON VariableConfig

instance FromJSON TemplateConfig where
  parseJSON = withObject "TemplateConfig" $ \v ->
    TemplateConfig
      <$> v .:? "variables" .!= M.empty
      <*> v .:? "preProcess" .!= []
      <*> v .:? "postProcess" .!= []
      <*> v .:? "description"
      <*> v .:? "overwrite" .!= []

getDefaultValue :: TemplateConfig -> T.Text -> Maybe T.Text
getDefaultValue config var = do
  valueConfig <- M.lookup var (variables config)
  defaultValue valueConfig

type VariableDefinitions = M.Map T.Text T.Text

defaultTemplateConfig :: TemplateConfig
defaultTemplateConfig = TemplateConfig M.empty [] [] Nothing []

data TemplateError
  = ParseError T.Text
  | DefinitionNotFoundError T.Text
  | UnsupportedTransform T.Text
  deriving (Eq)

instance Show TemplateError where
  show (ParseError err) = "Could not parse template: " ++ T.unpack err
  show (DefinitionNotFoundError var) = "Variable definition not found: " ++ T.unpack var
  show (UnsupportedTransform transform) = "Unsupported transform: " ++ T.unpack transform
