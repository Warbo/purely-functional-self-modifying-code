{-# LANGUAGE DeriveDataTypeable, OverlappingInstances, TypeFamilies #-}
{-# LANGUAGE UndecidableInstances                                   #-}
import Data.Data
import Data.Typeable
import Test.LazySmallCheck2012
import SelfMod

data ExprT = L | A ExprT ExprT | V ExprT | M
     deriving (Eq, Show, Typeable, Data)

instance Serial ExprT where
  series = cons2 A \/ cons1 V \/ cons0 M

ev :: ExprT -> Expr
ev L       = Lam id
ev (A l r) = App (ev l) (ev r)
ev (V x)   = Var (ev x)
ev M       = Mod

{-
instance Serial Expr where
  series = cons2 (\x y -> ev (A x y)) \/
           cons1 (ev . V)             \/
           cons0 Mod
-}

instance Serial Expr where
  series = cons1 Lam \/ cons2 App \/ cons1 Var \/ cons0 Mod

data Fix self = In (self (Fix self))

data BaseF a self = Either        (self -> self)
                          (Either (self,   self)
                                  (Either      self
                                               ()))

instance Argument Expr where
  type Base Expr = Fix (BaseF Expr)
  toBase e = case e of
                  Lam f   -> toBase . f . fromBase
                  App x y ->         Right . Left  $ (In (toBase x), In (toBase y))
                  Var x   -> Right . Right . Left  $  In (toBase x)
                  Mod     -> Right . Right . Right $ ()
  fromBase e = case e of
                    Left         f           -> Lam (fromBase . f . toBase)
                    Right (Left  (x, y))     -> App (fromBase x) (fromBase y)
                    Right (Right (Left  x))  -> Var (fromBase x)
                    Right (Right (Right ())) -> Mod

-- Test our Lambda Calculus implementation
encDec x = dec (enc x) == Just x

comparable (Lam _) = False
comparable _       = True

selfEvalTest x = let x' = eval x in
                 comparable x' ==> Just x' == dec (eval (App selfEval (enc x)))
