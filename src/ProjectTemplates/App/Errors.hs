{-# LANGUAGE StrictData #-}

module ProjectTemplates.App.Errors where

import Control.Exception (Exception)
import qualified Data.Text as T

newtype RunTimeError = RunTimeError T.Text
  deriving (Show)

newtype InternalError = InternalError T.Text
  deriving (Show)

instance Exception RunTimeError

instance Exception InternalError
