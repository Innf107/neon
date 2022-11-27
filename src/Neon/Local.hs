-- | This module defines the representation of local variables as used by
-- MIR and LIR. This is quite similar to a name in Neon, but uses variable indices
-- and makes generating new Locals and avoiding collisions much more robust.
-- As a bonus, comparisons on Locals should be significantly faster than on Names.
module Neon.Local (
    Local(..)
,   localIx
,   localName
) where

import Neon.Prelude
import Neon.Syntax (Name, renderName, renderNameNoPrefix) -- Ugh
import Neon.Pretty
import Neon.Config

data Local = Local {
    localIx :: Int
    -- localName is 'Just somename' iff this local was derived from a user-written variable.
    -- IMPORTANT: localName exists exclusively for debug information. Two locals with the same
    -- ix but different names will always behave the same way (except when displayed).
,   localName :: Maybe Name
}
instance Eq Local where (==) = (==) `on` localIx
instance Ord Local where compare = compare `on` localIx
instance Pretty Local where
    pretty (Local ix Nothing) = "_" <> show ix
    pretty (Local ix (Just name)) =
        let Config { printLocalPrefix } = getConfig () in
        if printLocalPrefix then
            renderName name <> "_" <> show ix
        else
            renderNameNoPrefix name <> "_" <> show ix
