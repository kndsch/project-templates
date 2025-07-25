{-# LANGUAGE StrictData #-}

module ProjectTemplates.App.Cli
  ( Options (..),
    cliParser,
  )
where

import qualified Data.Text as T
import Options.Applicative
import ProjectTemplates.App.Env (EnvVars (..))

data Options = Options
  { template :: Maybe T.Text,
    templatesDir :: FilePath,
    current :: Bool,
    verbosity :: Int,
    listTemplates :: Bool
  }
  deriving (Show)

optionParser :: EnvVars -> Parser Options
optionParser env =
  Options
    <$> optional (strArgument (metavar "TEMPLATE" <> help "Template to use"))
    <*> strOption
      ( long "template-dir"
          <> short 'd'
          <> metavar "TEMPLATE_DIR"
          <> value (defaultTemplatesDir env)
          <> showDefault
          <> help "Directory to write the output to"
      )
    <*> switch
      ( long "current"
          <> short 'c'
          <> help "Use the current directory as the project directory"
      )
    <*> option
      auto
      ( long "verbosity"
          <> short 'v'
          <> metavar "VERBOSITY"
          <> value 2
          <> showDefault
          <> help "Verbosity level (0-3)"
      )
    <*> switch
      ( long "list-templates"
          <> short 'l'
          <> help "List all available templates with descriptions"
      )

cliParser :: EnvVars -> ParserInfo Options
cliParser env =
  info
    (optionParser env <**> helper)
    (fullDesc <> progDesc "Generate a project from a template")
