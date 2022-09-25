module Calcite.Prelude (
    module Export
,   HSType
) where

import Relude as Export hiding (
        Type

    ,   fromException

    ,   Reader
    ,   ask
    ,   asks
    ,   local
    ,   runReader

    ,   State
    ,   runState
    ,   evalState 
    ,   execState
    ,   get
    ,   put
    ,   gets
    ,   modify
    ,   modify'

    ,   intercalate
    )
import Relude.Extra as Export
import Polysemy as Export
import Polysemy.Error as Export
import Polysemy.Reader as Export
import Polysemy.State as Export

import Data.Data as Export (Data)

import Data.Text as Export (intercalate, split)

import Data.Foldable as Export (foldrM)

import Relude qualified
type HSType = Relude.Type