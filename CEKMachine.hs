{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
import Data.Map

data Value = Boolean Bool
           | Number Int
             deriving (Show, Eq)

type Op2 = Value -> Value -> Expr

plus :: Op2
plus (Number x) (Number y) = Const $ Number (x + y)

mul :: Op2
mul (Number x) (Number y) = Const $ Number (x * y)

lt :: Op2
lt (Number x) (Number y) = Const $ Boolean (x < y)

instance Show (Op2) where
  show _ = "<<fun>>"

data Expr = Var String
          | Fun String Expr
          | Apply Expr Expr
          | Let String Expr Expr
          | BinOp Op2 Expr Expr
          | Const Value
          | If Expr Expr Expr
          | Pair Expr Expr
          | Fst Expr
          | Snd Expr
          | Fix String Expr
            deriving (Show)

data C = CS Expr
       | Halt
         deriving (Show)

type E = Map String Expr

data K = MT
       | Arg Expr E K
       | App String Expr E K
       | BinOpLeft Op2 Expr E K
       | BinOpRight Op2 Value E K
       | KLet String Expr K
         deriving (Show)

         
churn :: (C,E,K) -> (C,E,K)
churn (CS c, e, k) =
  let (c', e', k') = churn' (c, e, k)
  in (CS c', e', k')
churn _ = undefined "Cannot churn non Control String"

churn' :: (Expr, E, K) -> (Expr, E, K)
churn' (expr, env, k) =
  case (expr, k) of
    ((Let x y body), _) -> (y, env, KLet x body k)

    ((Const _), (KLet x body k)) -> 
      let env' = insert x expr env
      in (body, env', k)
    ((Const _), (App x body env k)) ->
      let env' = insert x expr env
      in (body, env', k)
    ((Const v), (BinOpLeft op e1 env k)) ->
      (e1, env, (BinOpRight op v env k))
    ((Const v1), (BinOpRight op v0 env k)) ->
      (op v0 v1, env, k)

    ((Var x), _) ->
      let expr = env ! x
      in (expr, env, k)

    ((BinOp op e0 e1), _) ->
      let k' = BinOpLeft op e1 env k
      in (e0, env, k')

    ((Apply e0 e1), k) ->
      let k' = Arg e1 env k
      in (e0, env, k')

    ((Fun _ _), (KLet x body k)) ->
      let env' = insert x expr env
      in (body, env', k)
    ((Fun x body), (Arg e env k)) ->
      let k' = App x body env k
      in (e, env, k')
    ((Fun _ _), (App x body env k)) ->
      let env' = insert x expr env
      in (body, env, k)
    
    _ -> error $ "Cannot churn: " ++ (show (expr, env, k))

makeButter :: (C,E,K) -> Either Value String
makeButter (CS (Const v), _, MT) = Left v
makeButter cek@(Halt, e, k) = Right $ "Butter making halted: " ++ (show cek)
makeButter cek = makeButter (churn cek)

run :: Expr -> Either Value String
run expr = makeButter (CS expr, empty, MT)

testShadowing =
  let test = Let "x" (Const (Number 5))
               (Let "plus5" (Fun "y" (BinOp plus (Var "x") (Var "y")))
                (Let "x" (Const (Number 10))
                 (Apply (Var "plus5") (Var "x"))))
  in (run test) == Left (Number 15)
