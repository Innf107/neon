{-# OPTIONS_GHC -Wno-orphans #-}
module Neon.Prelude (
    module Export
,   HSType
,   state
,   runOutputStdout
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
    ,   state

    ,   intercalate

    ,   unfoldr
    ,   tails
    ,   sortOn
    ,   sort
    ,   init
    ,   inits
    ,   replicateM
    ,   zip3
    ,   unzip
    ,   take
    ,   splitAt
    ,   scanr1
    ,   scanr
    ,   scanl1
    ,   scanl
    ,   reverse
    ,   replicate
    ,   drop
    ,   null
    ,   sortBy
    ,   zip
    ,   filter
    )
import Relude.Extra as Export
import Polysemy as Export
import Polysemy.Error as Export
import Polysemy.Reader as Export hiding (Local)
import Polysemy.State as Export
import Polysemy.Output as Export

import Data.Data as Export (Data)

import Data.Text as Export (intercalate, split)

import Data.Foldable as Export (foldrM)

import Data.Sequence as Export hiding (length, lookup, (!?), intersperse, zipWith, empty, fromList)
import Data.Sequence qualified as Seq

import Data.DList as Export (DList)

import Relude qualified
type HSType = Relude.Type

instance StaticMap (Seq a) where
    type Key (Seq a) = Int
    type Val (Seq a) = a
    size = length
    lookup = Seq.lookup
    member k s = maybe False (const True) $ lookup k s

state :: Member (State s) r => (s -> (a, s)) -> Sem r a
state f = do
    s <- get
    let (x, s') = f s
    put s'
    pure x

runOutputStdout :: Members '[Embed IO] r => (o -> Text) -> Sem (Output o : r) a -> Sem r a
runOutputStdout pretty = interpret \case
    Output o -> putTextLn (pretty o)