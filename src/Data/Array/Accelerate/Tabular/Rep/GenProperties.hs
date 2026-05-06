{-# LANGUAGE TemplateHaskellQuotes #-}

module Data.Array.Accelerate.Tabular.Rep.GenProperties (
  genProperty
, genProperties
) where

import Language.Haskell.TH
import Data.Typeable

-- | Generate functions for a unary type-level property of representations.
-- 
genProperty :: Name -> Q [Dec]
genProperty propName = do
  let propStr = nameBase propName

      -- Construct names
      proxyFun = mkName ("is" ++ propStr ++ "Proxy")
      metaFun  = mkName ("is" ++ propStr ++ "Meta")
      typeSyn  = mkName ("Is" ++ propStr)

  rep <- newName "rep"
  key <- newName "key"

  -- Types
  let propRep    = AppT (ConT propName) (VarT rep)
      eqType     = AppT (AppT (ConT ''(:~:)) propRep) (PromotedT 'True)
      maybeEq    = AppT (ConT ''Maybe) eqType

  -- isXProxy
  let proxySig =
        SigD proxyFun $
          ForallT [PlainTV rep SpecifiedSpec]
            [ AppT (ConT ''Typeable) propRep ]
            (AppT (AppT ArrowT (AppT (ConT ''Proxy) (VarT rep))) maybeEq)

      proxyFunDec =
        FunD proxyFun
          [ Clause [WildP]
              (NormalB (VarE 'eqT))
              []
          ]

  -- isXMeta
  let repConstraint =
        AppT (AppT (ConT (mkName "Rep")) (VarT rep)) (VarT key)

      metaType =
        AppT (ConT (mkName "Acc"))
             (AppT (AppT (ConT (mkName "Meta")) (VarT rep)) (VarT key))

      metaSig =
        SigD metaFun $
          ForallT [PlainTV rep SpecifiedSpec, PlainTV key SpecifiedSpec]
            [repConstraint]
            (AppT (AppT ArrowT metaType) maybeEq)

      metaFunDec =
        FunD metaFun
          [ Clause [WildP]
              (NormalB
                (AppE
                  (AppTypeE (VarE proxyFun) (VarT rep))
                  (ConE 'Proxy)))
              []
          ]

  -- type synonym
  let typeSynDec =
        TySynD typeSyn
          [PlainTV rep ()]
          (AppT (AppT EqualityT propRep) (PromotedT 'True))

  pure [proxySig, proxyFunDec, metaSig, metaFunDec, typeSynDec]

genProperties :: [Name] -> Q [Dec]
genProperties = fmap concat . mapM genProperty
