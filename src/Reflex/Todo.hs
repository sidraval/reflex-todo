{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}

module Reflex.Todo where

import Prelude hiding (mapM, mapM_, all, sequence)

import Reflex
import Reflex.Dom

main :: IO ()
main = mainWidget $ do
  input <- taskEntry
  someTasks <- foldDyn (:) [] input
  el "ul" $ taskList someTasks
  return ()

taskList :: MonadWidget t m => Dynamic t [String] -> m (Dynamic t [()])
taskList tasks = simpleList tasks $ el "li" . dynText

taskEntry :: MonadWidget t m => m (Event t String)
taskEntry = do
    el "header" $ do
      rec let newValueEnteredEvent = ffilter (==keycodeEnter) (_textInput_keypress descriptionBox)
          descriptionBox <- textInput $ def & setValue .~ fmap (const "") newValueEnteredEvent
      let newValue = tag (current $ _textInput_value descriptionBox) newValueEnteredEvent
      return newValue

