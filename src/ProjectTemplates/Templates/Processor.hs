{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module ProjectTemplates.Templates.Processor
  ( fillTemplate,
    collectVariables,
    parseValueDescription,
    TemplateProcessor,
    Template,
  )
where

import Control.Applicative (many, (<|>))
import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Char (isAlphaNum)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Void (Void)
import ProjectTemplates.Templates.Config (TemplateError (..), VariableDefinitions)
import ProjectTemplates.Templates.Transforms (applyTransform)
import Text.Megaparsec (MonadParsec (try), Parsec, between, runParser, takeWhile1P, (<?>))
import Text.Megaparsec.Char (char, space, string)
import Text.Megaparsec.Error (errorBundlePretty)

data ValueDescription = ValueDescription
  { name :: T.Text,
    transforms :: [T.Text]
  }

type Parser = Parsec Void T.Text

data TemplateChunk = Raw T.Text | Bracket ValueDescription

-- parse allowed chars and skip whitespace on either side
variableName :: Parser T.Text
variableName = space *> takeWhile1P (Just "variable name") validChar
  where
    validChar c = isAlphaNum c || c == '_' || c == '-'

valueParser :: Parser ValueDescription
valueParser = do
  name_ <- variableName
  space
  transforms_ <- many $ char '|' *> variableName
  space
  return $ ValueDescription name_ transforms_

opening :: Parser T.Text
opening = string "{{"

closing :: Parser T.Text
closing = string "}}"

bracketParser :: Parser ValueDescription
bracketParser = between opening closing valueParser <?> "Bracket"

nonBracket :: Parser TemplateChunk
nonBracket = Raw <$> takeWhile1P (Just "Non-Brackets") (/= '{')

tryBracket :: Parser TemplateChunk
tryBracket = try (Bracket <$> bracketParser) <|> Raw <$> takeWhile1P (Just "Non-matching brackets") (== '{')

templateParser :: Parser [TemplateChunk]
templateParser = many $ tryBracket <|> nonBracket

mapChunk :: VariableDefinitions -> TemplateChunk -> Either TemplateError T.Text
mapChunk _ (Raw t) = Right t
mapChunk vars (Bracket desc) = do
  value <- maybe notFoundError Right (M.lookup (name desc) vars)
  foldM applyTransform value (transforms desc)
  where
    notFoundError = Left $ DefinitionNotFoundError $ name desc

mapTemplate :: VariableDefinitions -> [TemplateChunk] -> Either TemplateError T.Text
mapTemplate config chunks = do
  mappedChunks <- traverse (mapChunk config) chunks
  return $ T.concat mappedChunks

type Template = T.Text

type TemplateProcessor a = Template -> Either TemplateError a

convertParser :: Parser a -> TemplateProcessor a
convertParser parser input = first convertError $ runParser parser "" input
  where
    convertError = ParseError . T.pack . errorBundlePretty

readChunks :: TemplateProcessor [TemplateChunk]
readChunks = convertParser templateParser

fillTemplate :: VariableDefinitions -> TemplateProcessor T.Text
fillTemplate vars input = do
  chunks <- readChunks input
  mapTemplate vars chunks

collectVariables :: TemplateProcessor [T.Text]
collectVariables input = mapMaybe getVariable <$> readChunks input
  where
    getVariable (Bracket desc) = Just $ name desc
    getVariable _ = Nothing

parseValueDescription :: TemplateProcessor ValueDescription
parseValueDescription = convertParser bracketParser
