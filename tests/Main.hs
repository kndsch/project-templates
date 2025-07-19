module Main where

import TemplatesSpec (spec_collectVariables, spec_fillTemplate)
import TransformsSpec (spec_applyTransform, spec_transformsInTemplates, spec_transformPerformance)
import ConfigSpec (spec)
import VariablesSpec (spec)
import ProcessSpec (spec)
import TemplateErrorSpec (spec)
import HooksSpec (spec)
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  templateSpecs <- traverse testSpecs [spec_collectVariables, spec_fillTemplate]
  transformSpecs <- traverse testSpecs [spec_applyTransform, spec_transformsInTemplates, spec_transformPerformance]
  configSpecs <- testSpecs ConfigSpec.spec
  variablesSpecs <- testSpecs VariablesSpec.spec
  processSpecs <- testSpecs ProcessSpec.spec
  templateErrorSpecs <- testSpecs TemplateErrorSpec.spec
  hooksSpecs <- testSpecs HooksSpec.spec
  defaultMain $ testGroup "All tests" 
    [ testGroup "Templates" (concat templateSpecs)
    , testGroup "Transforms" (concat transformSpecs)
    , testGroup "Config" configSpecs
    , testGroup "Variables" variablesSpecs
    , testGroup "Process" processSpecs
    , testGroup "Template Errors" templateErrorSpecs
    , testGroup "Hooks" hooksSpecs
    ]
