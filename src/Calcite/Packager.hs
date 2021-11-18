module Calcite.Packager (
    module Calcite.Packager
,   fromArchive
,   Archive
) where

import Calcite.Prelude

import Calcite.Types.AST

import Language.McFunction.Types hiding (Datapack)
import Language.McFunction.PrettyPrint

import Codec.Archive.Zip

data Datapack = Datapack {
    description :: Text
,   datapackNamespaces :: [DatapackNamespace]
} deriving (Show, Eq)

data DatapackNamespace = DatapackNamespace {
    namespace :: Text
,   functions :: [CompiledModule]
} deriving (Show, Eq)

package :: Text -> [CompiledModule] -> Datapack
package name funcs = Datapack {
        description = name
    ,   datapackNamespaces = [
            DatapackNamespace {
                namespace = name
            ,   functions = funcs
            }
        ]
    }

datapackToZip :: Datapack -> Archive
datapackToZip Datapack {description, datapackNamespaces} = foldr addEntryToArchive emptyArchive entries
    where
        entries = [
                (toEntry "pack.mcmeta" 0 (encodeUtf8 packMCMeta))
            ] <> concatMap packNamespace datapackNamespaces

        packNamespace (DatapackNamespace {namespace, functions}) = map packFunction functions
            where
                packFunction (name, cmnds) = toEntry 
                    ("data/" <> toString namespace <> "/functions/" <> name <> ".mcfunction") 
                    0 
                    (let ?namespace = namespace in encodeUtf8 $ snd $prettyPrint (name, cmnds))

        packMCMeta :: Text
        packMCMeta = unlines [
                "{"
            ,   "    \"pack\":{"
            ,   "       \"pack_format\": 7,"
            ,   "       \"description\": \"" <> description <> "\""
            ,   "    }"
            ,   "}"
            ]

