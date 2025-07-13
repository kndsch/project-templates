module ProjectTemplates.Templates.Transforms (applyTransform) where

import qualified Data.Text as T
import ProjectTemplates.Templates.Config (TemplateError (..))
import Text.Casing (fromAny, toCamel, toKebab, toPascal, toSnake)

parseTransformName :: T.Text -> Either TemplateError (T.Text -> T.Text)
parseTransformName name = case T.toLower name of
  "camelcase" -> wrapTransform toCamel
  "snakecase" -> wrapTransform toSnake
  "kebabcase" -> wrapTransform toKebab
  "pascalcase" -> wrapTransform toPascal
  "uppercase" -> Right T.toUpper
  "lowercase" -> Right T.toLower
  _ -> Left $ UnsupportedTransform name
  where
    wrapTransform f = Right $ T.pack . f . fromAny . T.unpack

applyTransform :: T.Text -> T.Text -> Either TemplateError T.Text
applyTransform value name = do
  transform <- parseTransformName name
  return $ transform value
