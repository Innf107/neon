module Neon.Span (
    Span (..),
    Spanned (..),
) where

-- This imports @Prelude@ instead of @Neon.Prelude@ to avoid cyclical dependencies between
-- @Neon.Span@ and @Neon.Prelude@, since the latter reexports the former.
import Prelude

data Span = UnsafeMkSpan
    { sourceFile :: FilePath
    , startCol :: Int
    , startLine :: Int
    , endCol :: Int
    , endLine :: Int
    }
    deriving (Eq)

instance Show Span where
    show (UnsafeMkSpan{sourceFile, startCol, startLine, endCol, endLine}) =
        -- We include a space between the start and end position to allow tools like VSCode to
        -- detect source positions as clickable links. Without the space, VSCode would not recognize
        -- '25:16-25:17' as a valid source position
        sourceFile <> ":" <> show startLine <> ":" <> show startCol <> " - " <> show endLine <> ":" <> show endCol

class Spanned a where
    spanOf :: a -> Span

instance Spanned Span where
    spanOf = id