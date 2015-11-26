{-# LANGUAGE DeriveDataTypeable, OverlappingInstances, TypeFamilies #-}
{-# LANGUAGE UndecidableInstances, TemplateHaskell, TypeOperators   #-}
{-# LANGUAGE StandaloneDeriving, TypeSynonymInstances, FlexibleInstances #-}

module Tests where

import Data.Data
import Data.Typeable
import Test.LazySmallCheck2012
import Test.LazySmallCheck2012.FunctionalValues
import Test.LazySmallCheck2012.TH
import SelfMod

-- TODO: Extend the Lam case to more interesting functions
sExpr :: Series Expr
sExpr = pure  Mod               <|>
        pure (Lam id)           <|>
        App <$> sExpr <*> sExpr <|>
        Var <$> sExpr

-- Test our Lambda Calculus implementation
encDec x = dec (enc x) == Just x

comparable (Lam _) = False
comparable _       = True

selfEvalTest x = let x' = eval x in
                 comparable x' ==> Just x' == dec (eval (App selfEval (enc x)))
