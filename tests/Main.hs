module Main where

import TemplatesSpec (spec_collectVariables, spec_fillTemplate)
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  specs <- traverse testSpecs [spec_collectVariables, spec_fillTemplate]
  defaultMain $ testGroup "All tests" [testGroup "Templates" (concat specs)]
