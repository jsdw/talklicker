{-# LANGUAGE TemplateHaskell #-}

module Types.Lenses where

import Types.Lenses.MkLens
import Types.Types

--
-- This module generates all of our lenses for our types.
-- it exports *everything*, so we don't have to manually do it.
--
makeLensesL ''User
makeLensesL ''Entry
makeLensesL ''Day
makeLensesL ''Everything

makeLensesL ''UserInput
makeLensesL ''DayInput
makeLensesL ''LoginInput
makeLensesL ''UserOutput