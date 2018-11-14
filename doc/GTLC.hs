module GTLC where

{- An implementation of the gradually typed lambda calculus from the
   original gradual types paper:
   http://scheme2006.cs.uchicago.edu/13-siek.pdf
-}

import Control.Monad

type Var = Int
type Const = String

data Type
  = Int | Bool
  | Arrow Type Type
  | Untyped
    -- Reference type
  | Ref Type
    deriving (Show,Eq)

data Exp
  = Var Var
  | Const Const
  | Lam Var Type Exp
  | App Exp Exp
    -- References
  | MkRef Exp | Deref Exp | Assign Exp Exp
    -- Used in the operational semantics
  | Cast Type Exp
    deriving (Show,Eq)

consistent :: Type -> Type -> Bool
consistent t1 t2 | t1 == t2 = True
consistent Untyped _ = True
consistent _ Untyped = True
consistent (Arrow s1 s2) (Arrow t1 t2) = consistent s1 t1 && consistent s2 t2
consistent _ _ = False

type Env = ([(Const,Type)], [(Var,Type)])

typeCheck :: Env -> Exp -> Maybe Type
typeCheck (_,vEnv) (Var v) = lookup v vEnv
typeCheck (cEnv,_) (Const c) = lookup c cEnv
typeCheck (cEnv,vEnv) (Lam x t e) = do
  s <- typeCheck (cEnv,(x,t):vEnv) e
  return (Arrow t s)
typeCheck env (App e1 e2) = do
  t1 <- typeCheck env e1
  t2 <- typeCheck env e2
  case t1 of
    Untyped -> do
      return Untyped
    Arrow t' t3 -> do
      if consistent t' t2
        then return t3
        else fail "Type error in application"
    _ ->
      fail "Non-function type"
typeCheck env (MkRef e) = do
  t <- typeCheck env e
  return (Ref t)
typeCheck env (Deref e) = do
  t <- typeCheck env e
  case t of
    Ref t' -> return t'
    Untyped -> return Untyped
    _ -> fail "Type error in deref"
typeCheck env (Assign e1 e2) = do
  s <- typeCheck env e1
  t <- typeCheck env e2
  case s of
    Untyped -> return Untyped
    Ref t' -> do
      guard (consistent t' t)
      return s
    _ -> fail "Type error in assign"

example = typeCheck ([("succ",Arrow Int Int),("hi",Bool)],[])
  (App (Const "succ") (Const "hi"))
variation = typeCheck ([("succ",Arrow Int Int),("hi",Untyped)],[])
  (App (Const "succ") (Const "hi"))

translate :: Env -> Exp -> Maybe (Exp, Type)
translate (_, vEnv) e@(Var v) = do
  t <- lookup v vEnv
  return (e,t)
translate (cEnv, _) e@(Const c) = do
  t <- lookup c cEnv
  return (e,t)
translate (cEnv, vEnv) e@(Lam x t eb) = do
  (eb', s) <- translate (cEnv, (x,t):vEnv) eb
  return (Lam x t eb', Arrow t s)
translate env (App e1 e2) = do
  (e1', t) <- translate env e1
  (e2', s) <- translate env e2
  case t of
    Untyped -> do
      return (App (Cast (Arrow s Untyped) e1') e2', Untyped)
    Arrow m n | s /= m -> do
      guard (consistent s m)
      return (App e1' (Cast m e2'), n)
    Arrow m n -> do
      return (App e1' e2', n)
    _ ->
      fail "Type error in application"
translate env (MkRef e) = do
  (e', t) <- translate env e
  return (MkRef e', Ref t)
translate env (Deref e) = do
  (e', t) <- translate env e
  case t of
    Untyped -> do
      return (Deref (Cast (Ref Untyped) e'), Untyped)
    Ref t' -> do
      return (Deref e', t')
    _ -> do
      fail "Type error in deref"
translate env (Assign e1 e2) = do
  (e1', t) <- translate env e1
  (e2', s) <- translate env e2
  case t of
    Untyped -> do
      return (Assign (Cast (Ref s) e1') e2', Ref s)
    Ref t' | t' /= s -> do
      guard (consistent t' s)
      return (Assign e1' (Cast t' e2'), t)
    Ref _ ->
      return (Assign e1' e2', t)
    _ ->
      fail "Type error in assign"

-- TODO: Value and operational semantics

