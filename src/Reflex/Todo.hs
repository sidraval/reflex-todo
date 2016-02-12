{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}
module Reflex.Todo where

import Prelude hiding (mapM, mapM_, all, sequence)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable
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
  rec tasks <- foldDyn ($) initialTasks $ mergeWith (.) [fmap insertNew newTask, listModifyTasks]
      newTask <- taskEntry
      listModifyTasks <- taskList tasks
  return ()

taskList :: (MonadWidget t m, Ord k) => Dynamic t (Map k String) -> m (Event t (Map k String -> Map k String))
taskList tasks = do
    items <- el "ul" $ list tasks todoItem
    let itemChange = fmap (foldl' (.) id) . mergeList . map (\(k, v) -> fmap (flip Map.update k) v) . Map.toList
    itemChangeEvent <- mapDyn itemChange items
    return $ switch . current $ itemChangeEvent

todoItem :: MonadWidget t m => Dynamic t String -> m (Event t (String -> Maybe String))
todoItem task = do
    el "li" $ dynText task
    removeClicked <- button "remove"
    return $ fmap (const $ const Nothing) removeClicked

taskEntry :: MonadWidget t m => m (Event t String)
taskEntry = do
    el "header" $ do
      rec let newValueEnteredEvent = ffilter (==keycodeEnter) (_textInput_keypress descriptionBox)
          descriptionBox <- textInput $ def & setValue .~ fmap (const "") newValueEnteredEvent
      let newValue = tag (current $ _textInput_value descriptionBox) newValueEnteredEvent
      return newValue
