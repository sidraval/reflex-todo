{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}
module Reflex.Todo where

import Prelude hiding (mapM, mapM_, all, sequence)

import GHCJS.DOM.Element
import Control.Monad hiding (mapM, mapM_, forM, forM_, sequence)
import Control.Monad.Trans
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable
import Data.Monoid ((<>))
import Data.List (intercalate)
import Data.FileEmbed
import Control.Concurrent
import qualified Data.Text as T

import Reflex
import Reflex.Dom

data Task = Task { taskDescription :: String } deriving (Show, Eq)

main :: IO ()
main = mainWidget $ do
  input <- taskEntry
  someTasks <- foldDyn (\el accum -> el:accum) [] input
  el "ul" $ taskList someTasks
  return ()

taskList :: MonadWidget t m => Dynamic t [String] -> m (Dynamic t [()])
taskList tasks = simpleList tasks (\dynTask -> el "li" $ dynText dynTask)

taskEntry :: MonadWidget t m => m (Event t String)
taskEntry = do
    el "header" $ do
      rec let newValueEnteredEvent = ffilter (==keycodeEnter) (_textInput_keypress descriptionBox)
          descriptionBox <- textInput $ def & setValue .~ fmap (const "") newValueEnteredEvent
      let newValue = tag (current $ _textInput_value descriptionBox) newValueEnteredEvent
      return newValue

