module Neon.Util where

import Neon.Prelude
import Language.Haskell.TH as TH

forAllInstances :: Name -> (Name -> DecsQ) -> DecsQ
forAllInstances name makeFun = do
    reify name >>= \case
        ClassI _ instanceDecs -> do
            concat <$> forM instanceDecs \case
                (InstanceD _ _ (AppT _ (ConT con)) _) ->
                    makeFun con
                inst -> fail $ "forAllInstances: invalid type class instance:" <> show inst
        _ -> fail "forAllInstances only works on type classes"


