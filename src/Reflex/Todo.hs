{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}
module Reflex.Todo where

module Reflex.Todo where

import Prelude hiding (mapM, mapM_, all, sequence)

import Data.Map (Map)
import qualified Data.Map as Map
import Reflex
import Reflex.Dom

insertNew :: (Enum k, Ord k) => v -> Map k v -> Map k v
insertNew v m = case Map.maxViewWithKey m of
  Nothing -> Map.singleton (toEnum 0) v
  Just ((k, _), _) -> Map.insert (succ k) v m

initialTasks :: Map Int String
initialTasks = Map.empty

main :: IO ()
main = mainWidget $ do
  input <- taskEntry
  someTasks <- foldDyn insertNew initialTasks input
  el "ul" $ taskList someTasks
  return ()

taskList :: (MonadWidget t m, Ord k) => Dynamic t (Map k String) -> m (Dynamic t (Map k ()))
taskList tasks = list tasks $ el "li" . dynText

taskEntry :: MonadWidget t m => m (Event t String)
taskEntry = do
    el "header" $ do
      rec let newValueEnteredEvent = ffilter (==keycodeEnter) (_textInput_keypress descriptionBox)
          descriptionBox <- textInput $ def & setValue .~ fmap (const "") newValueEnteredEvent
      let newValue = tag (current $ _textInput_value descriptionBox) newValueEnteredEvent
      return newValue
