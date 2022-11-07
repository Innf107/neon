{-# LANGUAGE QuasiQuotes, TemplateHaskellQuotes, TemplateHaskell #-}
module Neon.Pretty where

-- TODO: Move to GADT syntax

import Neon.Prelude
import GHC.TypeLits (Symbol)

import Language.Haskell.TH

import Prelude qualified
import Data.List qualified as List
import Data.Char (isDigit)

type PrettyAnn (s :: Symbol) a = a

class Pretty a where
    pretty :: a -> Text

instance Pretty Int where pretty = show
instance Pretty Text where pretty = show

makePretty :: Name -> DecsQ
makePretty ty = do
    reify ty >>= \case
        -- TODO: Include type variables in instance
        TyConI (NewtypeD _cxt name tvs _mkind con _deriv) -> buildPrettyInst name tvs [con]
        TyConI (DataD _cxt name tvs _mkind cons _deriv) -> buildPrettyInst name tvs cons
        _ -> fail "makePretty only works on regular data types and newtypes for now"
    where
        buildPrettyInst name tvs cons = do                      
            let go conName argTys = do
                    template <- case argTys of
                        ((AppT (AppT (ConT annCon) (LitT (StrTyLit template))) _) : _)
                            | annCon == ''PrettyAnn -> pure template
                        _ -> fail "For makePretty to work, all data constructors need to start with a type of the form `PrettyAnn \"something\" t`"
                    
                    -- This includes an underscore so that the argument can be ignored without generating a warning
                    names <- traverse (\_ -> newName "_x") argTys
                    
                    expr <- replaceTemplateNames template names

                    let pat = ConP conName (map VarP names)

                    pure (Match pat (NormalB expr) [])
            let goGADT conName argTys retTy = do
                    template <- case retTy of
                        (AppT (AppT (ConT annCon) (LitT (StrTyLit template))) _)
                            | annCon == ''PrettyAnn -> pure template
                        _ -> fail "For makePretty to work, all GADT data constructors need return a type of the form `PrettyAnn \"something\" t`"
                    
                    -- This includes an underscore so that the argument can be ignored without generating a warning
                    names <- traverse (\_ -> newName "_x") argTys
                    
                    expr <- replaceTemplateNames template names

                    let pat = ConP conName (map VarP names)

                    pure (Match pat (NormalB expr) [])
            matches :: [Match] <- forM cons \case            
                NormalC conName argBangTys -> go conName (map snd argBangTys) -- Why does Haskell not have or patterns?
                RecC conName varBangTys -> go conName (map (\(_, _, ty) -> ty) varBangTys)
                GadtC [conName] bangTys retTy -> goGADT conName (map snd bangTys) retTy
                RecGadtC [conName] varBangTys retTy -> goGADT conName (map (\(_, _, ty) -> ty) varBangTys) retTy
                _ -> fail "makePretty does not work on infix- or forall- data constructors for now"
                
            let case_ = caseE (varE (mkName "x")) (map pure matches)
            [d| instance Pretty $(conT name) where 
                    pretty $(varP (mkName "x")) = $case_
                |]

-- TODO: Update error messages

replaceTemplateNames :: String -> [Name] -> Q Exp
replaceTemplateNames template names = go "" template
    where
        go acc "" = pure $ makeString acc
        go acc ('$' : rest)
            | (index@(_ : _), rest) <- List.span isDigit rest = do
                name <- getName (Prelude.read index) names
                case rest of
                    ('*' : '\'': rest) -> do
                        (separator, rest) <- pure $ List.span (/= '\'') rest
                        case rest of
                            ('\'' : rest) -> 
                                [|$(pure (makeString acc)) <> intercalate $(litE (stringL separator)) (List.map pretty (toList $(varE name))) <> $(go [] rest)|]
                            _ -> fail "makePretty: undelimited separator in repeating template pattern"
                    _ -> [|$(pure (makeString acc)) <> pretty $(varE name) <> $(go [] rest)|]
        go acc (c : rest) = go (c : acc) rest

        makeString acc = LitE (StringL (List.reverse acc))

        getName :: Int -> [Name] -> Q Name
        getName originalIndex = go originalIndex
            where
                go 0 (x : _) = pure x
                go i (_ : xs) = getName (i - 1) xs
                go _ [] = fail $ "makePretty: data constructor does not have an argument at index " <> show originalIndex <> ". It only has " <> show (length names) <> " arguments."

