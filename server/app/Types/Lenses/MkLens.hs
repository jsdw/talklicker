{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Types.Lenses.MkLens (makeLensesL) where

import Lens.Micro.Platform
import Language.Haskell.TH.Syntax (nameBase, mkName)

--
-- Our custom mkLens function makes lenses for all fields and
-- just appends an L to the end of it. no underscores required.
--
makeLensesL = makeLensesWith (lensRules & set lensField makeName)
  where makeName _ _ n = [TopName (mkName (nameBase n ++ "L"))]