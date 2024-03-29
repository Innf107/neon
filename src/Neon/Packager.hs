module Neon.Packager (datapackToZip) where

import Neon.Prelude

import Codec.Archive.Zip


datapackToZip :: Foldable f => Text -> f (FilePath, Text) -> Archive
datapackToZip packName datapack =
    foldr addFunction skeletonArchive datapack
        where
            addFunction (path, content) archive =
                let fullPath = "data/" <> toString packName <> "/functions/" <> path in
                addEntryToArchive (toEntry fullPath 0 (encodeUtf8 content)) archive

skeletonArchive :: Archive
skeletonArchive =
      addEntryToArchive (toEntry "pack.mcmeta" 0 defaultMCMeta) 
    $ emptyArchive

defaultMCMeta :: LByteString
defaultMCMeta = 
        "{\n"
    <>  "    \"pack\":{\n"
    <>  "        \"pack_format\":7,\n"
    <>  "        \"description\": \"Generated by Neon\"\n"
    <>  "    }\n"
    <>  "}\n"

