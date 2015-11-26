> {-# LANGUAGE DeriveDataTypeable, OverlappingInstances, StandaloneDeriving, FlexibleInstances #-}
> module SelfMod where
> import Data.Number.Nat
> import Data.Data
> import Data.Typeable
> import Control.Applicative

Self-modification can be modelled using the following components:

 * A driver mechanism to compute a result from our code
 * Read access to a representation of the running code
 * A mechanism to switch to running a different representation

First we need a language to program in. For the sake of tradition, we'll use
Lambda Calculus, augmented with a code-switching primitive. This was inspired
by http://lambda-the-ultimate.org/node/3075#comment-44805

> data Expr = Lam (Expr -> Expr)  -- Lambda abstraction and its body
>           | App Expr Expr       -- Application
>           | Var Expr            -- Variable with a value
>           | Mod                 -- Self-modify primitive function
>           deriving (Typeable)

> instance Eq Expr where
>   App a b == App x y = a == x && b == y
>   Var x   == Var y   = x == y
>   Mod     == Mod     = True
>   Lam f   == Lam g   = False
>   _       == _       = False

> instance Show Expr where
>   show (App a b) = "(" ++ show a ++ " " ++ show b ++ ")"
>   show (Var x)   = 'V' : show x
>   show (Mod)     = "Mod"
>   show (Lam f)   = "λ..."

Evaluating such expressions is straightforward:

> eval e = let app (Lam f) x = eval (f x)  -- Call a lambda's body
>              app Mod  x = eval (App selfEval x)
>              app      f  x = App   f x   -- Don't apply non-lambdas
>          in case e of
>               Var   v -> Var v           -- Variables don't reduce
>               Lam   b -> Lam b           -- Lambdas don't reduce
>               Mod     -> Mod             -- Mod doesn't reduce
>               App l r -> app (eval l) r  -- Eval the applicand and apply it

Now that we've embedded Lambda Calculus in Haskell, we need a way to embed
Lambda Calculus in Lambda Calculus. We do this using Morgensen-Scott encoding:

> class Encodable a where
>   enc :: a -> Expr
>   dec :: Expr -> Maybe a

> instance Encodable Expr where
>   enc e = Lam $ \a -> Lam $ \b -> Lam $ \c -> Lam $ \d -> case e of
>             Lam f   -> App a (Lam f)
>             Var x   -> App b (enc x)
>             App x y -> App (App c (enc x)) (enc y)
>             Mod     -> d
>   dec e = case eval (App (App (App (App e (Lam id))
>                                    (Lam (Var . Var)))
>                               (Lam $ \x -> Lam $ \y -> Var (App x y)))
>                           Mod) of
>             Var (Var x)   -> fmap Var (dec x)
>             Var (App x y) -> App <$> dec x <*> dec y
>             x             -> Just x

> selfEval = Lam $ \x -> let Just x' = dec x in enc (eval x')

We'll use ASTs to represent code, which we can implement as a free monad:

type AST = Free Expr

Now we

Now we need a way to evaluate ASTs in an environment:
