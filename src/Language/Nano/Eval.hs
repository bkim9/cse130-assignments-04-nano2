{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Language.Nano.Eval
  ( execFile, execString, execExpr
  , eval, lookupId, prelude
  , parse
  , env0
  )
  where

import Control.Exception (throw, catch)
import Language.Nano.Types
import Language.Nano.Parser

--------------------------------------------------------------------------------
execFile :: FilePath -> IO Value
--------------------------------------------------------------------------------
execFile f = (readFile f >>= execString) `catch` exitError

--------------------------------------------------------------------------------
execString :: String -> IO Value
--------------------------------------------------------------------------------
execString s = execExpr (parseExpr s) `catch` exitError

--------------------------------------------------------------------------------
execExpr :: Expr -> IO Value
--------------------------------------------------------------------------------
execExpr e = return (eval prelude e) `catch` exitError

--------------------------------------------------------------------------------
-- | `parse s` returns the Expr representation of the String s
--
-- >>> parse "True"
-- EBool True
--
-- >>> parse "False"
-- EBool False
--
-- >>> parse "123"
-- EInt 123
--
-- >>> parse "foo"
-- EVar "foo"
--
-- >>> parse "x + y"
-- EBin Plus (EVar "x") (EVar "y")
--
-- >>> parse "if x <= 4 then a || b else a && b"
-- EIf (EBin Le (EVar "x") (EInt 4)) (EBin Or (EVar "a") (EVar "b")) (EBin And (EVar "a") (EVar "b"))
--
-- >>> parse "if 4 <= z then 1 - z else 4 * z"
-- EIf (EBin Le (EInt 4) (EVar "z")) (EBin Minus (EInt 1) (EVar "z")) (EBin Mul (EInt 4) (EVar "z"))
--
-- >>> parse "let a = 6 * 2 in a /= 11"
-- ELet "a" (EBin Mul (EInt 6) (EInt 2)) (EBin Ne (EVar "a") (EInt 11))
--
-- >>> parseTokens "() (  )"
-- Right [LPAREN (AlexPn 0 1 1),RPAREN (AlexPn 1 1 2),LPAREN (AlexPn 3 1 4),RPAREN (AlexPn 6 1 7)]
--
-- >>> parse "f x"
-- EApp (EVar "f") (EVar "x")
--
-- >>> parse "(\\ x -> x + x) (3 * 3)"
-- EApp (ELam "x" (EBin Plus (EVar "x") (EVar "x"))) (EBin Mul (EInt 3) (EInt 3))
--
-- >>> parse "(((add3 (x)) y) z)"
-- EApp (EApp (EApp (EVar "add3") (EVar "x")) (EVar "y")) (EVar "z")
--
-- >>> parse <$> readFile "tests/input/t1.hs"
-- EBin Mul (EBin Plus (EInt 2) (EInt 3)) (EBin Plus (EInt 4) (EInt 5))
--
-- >>> parse <$> readFile "tests/input/t2.hs"
-- ELet "z" (EInt 3) (ELet "y" (EInt 2) (ELet "x" (EInt 1) (ELet "z1" (EInt 0) (EBin Minus (EBin Plus (EVar "x") (EVar "y")) (EBin Plus (EVar "z") (EVar "z1"))))))
--
-- >>> parse "1-2-3"
-- EBin Minus (EBin Minus (EInt 1) (EInt 2)) (EInt 3)
-- >>> parse "1+a&&b||c+d*e-f-g x"
-- EBin Or (EBin And (EBin Plus (EInt 1) (EVar "a")) (EVar "b")) (EBin Minus (EBin Minus (EBin Plus (EVar "c") (EBin Mul (EVar "d") (EVar "e"))) (EVar "f")) (EApp (EVar "g") (EVar "x")))
--
-- >>> parse "1:3:5:[]"
-- EBin Cons (EInt 1) (EBin Cons (EInt 3) (EBin Cons (EInt 5) ENil))
--
-- >>> parse "[1,3,5]"
-- EBin Cons (EInt 1) (EBin Cons (EInt 3) (EBin Cons (EInt 5) ENil))

--------------------------------------------------------------------------------
parse :: String -> Expr
--------------------------------------------------------------------------------
parse = parseExpr

exitError :: Error -> IO Value
exitError (Error msg) = return (VErr msg)

--------------------------------------------------------------------------------
-- | `eval env e` evaluates the Nano expression `e` in the environment `env`
--   (i.e. uses `env` for the values of the **free variables** in `e`),
--   and throws an `Error "unbound variable"` if the expression contains
--   a free variable that is **not bound** in `env`.
--
-- >>> eval env0 (EBin Minus (EVar "x") (EVar "y"))
-- -1
--
-- >>> eval env0 (EBin Minus (EBin Plus (EVar "x") (EVar "y")) (EBin Plus (EVar "z") (EVar "z1")))
-- 0
--
-- >>> eval env0 (EVar "p")
-- Error {errMsg = "unbound variable: p"}
--
-- >>> eval []  (EBin Le (EInt 2) (EInt 3))
-- True
--
-- >>> eval []  (EBin Eq (EInt 2) (EInt 3))
-- False
--
-- >>> eval []  (EBin Eq (EInt 2) (EBool True))
-- Error {errMsg = "type error: binop"}
--
-- >>> eval []  (EBin Lt (EInt 2) (EBool True))
-- Error {errMsg = "type error: binop"}
--
-- >>> eval env0 (EBin Lt (EVar "z1") (EVar "x"))
-- True

-- >>> eval env0 (EBin Ne (EVar "y") (EVar "z"))
-- True

-- >>> let e1 = EIf (EBin Lt (EVar "z1") (EVar "x")) (EBin Ne (EVar "y") (EVar "z")) (EBool False)
-- >>> eval env0 e1
-- True
--
-- >>> let e2 = EIf (EBin Eq (EVar "z1") (EVar "x")) (EBin Le (EVar "y") (EVar "z")) (EBin Le (EVar "z") (EVar "y"))
-- >>> eval env0 e2
-- False
--
-- part (c)
--
-- >>> let e1 = EBin Plus (EVar "x") (EVar "y")
-- >>> let e2 = ELet "x" (EInt 1) (ELet "y" (EInt 2) e1)
-- >>> eval [] e2
-- 3
--
-- part (d)
--
-- >>> eval [] (EApp (ELam "x" (EInt 3)) (EInt 2) ) 
-- 3

-- >>> eval [] (ELam "x" (EVar "x"))
-- <<{  }, \x -> EVar "x">>

-- >>> eval [] (EApp (ELam "x" (EVar "x")) (EInt 3))
-- 3

-- >>> eval [] (EBin Plus (EVar "x") (EVar "x"))
-- Error {errMsg = "unbound variable: x"}

-- >>> eval [] (ELam "x" (EBin Plus (EVar "x") (EVar "x")))
-- <<{  }, \x -> EBin Plus (EVar "x") (EVar "x")>>

-- >>> eval [] (EApp (ELam "x" (EBin Plus (EVar "x") (EVar "x"))) (EInt 3))
-- 6
--
-- >>> eval [] (EApp (ELam "x" (EBin Plus (EVar "x") (EVar "x"))) (EInt 3))
-- 6

-- >>> let e3 = ELet "h" (ELam "y" (EBin Plus (EVar "x") (EVar "y"))) (EApp (EVar "f") (EVar "h"))
-- >>> let e2 = ELet "x" (EInt 100) e3
-- >>> let e1 = ELet "f" (ELam "g" (ELet "x" (EInt 0) (EApp (EVar "g") (EInt 2)))) e2
-- >>> eval [] e1
-- 102

-- part (e)
-- 
---
--
-- >>>eval [] (EApp (ELam "n" (EIf (EBin Eq (EVar "n") (EInt 0)) (EInt 9) (EInt 2) ) ) (EInt 0))
-- 9
--
-- eval env2 e8 = eval env2 (EVar "fac") = eval (("n"   ,eval env1 e5):[]) (EVar "fac") = 
------ NO "fac" IN env2!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!? env1 has one though



-- >>> eval [] (ELet "fac" (ELam "n" (EIf (EBin Eq (EVar "n") (EInt 0)) (EInt 1) (EBin Mul (EVar "n") (EApp (EVar "fac") (EBin Minus (EVar "n") (EInt 1)))))) (EApp (EVar "fac") (EInt 10)))
-- 3628800
-- 
-- part (f)
--
-- >>> let el = EBin Cons (EInt 1) (EBin Cons (EInt 2) ENil)
-- >>> execExpr el
-- (1 : (2 : []))

--- >>> let el = EBin Cons (EInt 1) (EBin Cons (EInt 2) ENil)
--- >>> eval prelude el
-- (1 : (2 : []))

--- >>> let el = EBin Cons (EInt 1) (EBin Cons (EInt 2) ENil)
-- >>> execExpr (EApp (EVar "head") el)
-- 1
-- 
-- >>> execExpr (EApp (EVar "tail") el)
-- (2 : [])
--------------------------------------------------------------------------------
eval :: Env -> Expr -> Value
--------------------------------------------------------------------------------

eval _ (EInt n) = VInt n
eval _ (EBool b) = VBool b 
eval _ ENil = VNil
eval env (EVar x) = lookupId x env
eval env (EBin op e1 e2) = evalOp op (eval env e1) (eval env e2)
eval env (EIf p t f)
   | eval env p == VBool True =eval env t  --pattern matching
   | otherwise = eval env f
eval env (ELet x e1 e2)= eval ((x, eval env e1):env) e2
eval env (ELam x e) = VClos env x e
eval prelude (EApp (EVar "head") el) = vp (eval prelude el)
  where
    vph =  VPrim (\(VCons (VInt head) _) -> VInt head)
    VPrim vp = eval [("prim",vph)] (EVar "prim")
eval prelude (EApp (EVar "tail") el) = vp (eval prelude el)
  where
    vpt =  VPrim (\(VCons (VInt _) tail) -> tail)
    VPrim vp = eval [("prim",vpt)] (EVar "prim")
eval env (EApp (EVar f) e2) = eval ((f, eval env (EVar f)):((binder,eval env e2):env_old)) body
   where
     VClos env_old binder body = eval env (EVar f)
eval env (EApp e1 e2) = eval ((binder,eval env e2):env_old) body
   where
     VClos env_old binder body = eval env e1
-- eval prelude ((EVar p) (EVar xs)) = VInt (vp (eval prelude xs))
--   where 
--      vph = eval prelude "head"
--      VPrim vp = eval [("prim", vph), ("xs", xs)] p



--                 3 : xs
--           (Value ) (Value ( VInt ) VNil)
-- >>> VCons (VInt 3) (VCons (VInt 2) VNil)
-- (3 : (2 : []))

-- >>> let head = VInt 6
-- >>> let tail = VCons (VInt 3) (VCons (VInt 2) VNil)
-- >>> VCons head tail
-- (6 : (3 : (2 : [])))


--- >>> VCons head tail = VCons (VInt 3) (VCons (VInt 2) VNil)
--- >>> show head
--- >>> tail
-- "3"
-- (2 : [])

-- >>> VCons head tail = VCons (VInt 3) (VCons (VInt 2) VNil)
-- >>> VPrim ((VCons (VInt head) tail) ->(VInt head))
-- Pattern syntax in expression context:
--             (VCons (VInt head) tail) -> (VInt head)


-- >>> VCons (VInt 2) VNil
-- (2 : [])


--- >>> prim = VPrim ((VCons (VInt head) tail) -> (VInt head))
--  >>> header = prim (VCons (VInt 2) VNil)
-- Pattern syntax in expression context:
--     (VCons (VInt head) tail) -> (VInt head)



--- >>> VPrim (\(VCons (VInt head) tail) -> (VInt head))
-- <<primitive-function>>

-- >>>(\(VCons (VInt head) tail) -> (VInt head)) (VCons (VInt 2) VNil)
-- 2

-- >>> (\(VCons (VInt head) tail) -> (tail)) (VCons (VInt 2) VNil)
-- []







---  execExpr e                       = return (eval prelude e                      ) `catch` exitError
-- = execExpr (EApp (EVar "head") el) = return (eval prelude (EApp (EVar "head") el)) `catch` exitError

---  execExpr e                       = return (eval prelude e                       )
-- >>>  execExpr (EApp (EVar "tail") el) = return (Eval prelude (EApp (EVar "tail")) el)
-- Data constructor not in scope:
--   Eval :: Env -> (Expr -> Expr) -> Expr -> a

--- >>> let el = EBin Cons (EInt 1) ENil
--- >>> VCons head tail = eval [] el
--- >>> es = (EApp (EVar "head") el)
--- >>> let prelude = [("head",head)]
--- >>> head
--- >>> tail
--- >>> VCons head2 tail2 = eval prelude es
--- >>> head2
--- >>> VCons head3 tail3 = eval prelude (EApp (EVar "head") el)
--- >>> VClos env_old binder body = eval prelude (EVar "head")

--- >>> VClos env_old binder body = eval [("head", VClos env_old binder body)] (EVar "head")

--- >>> eval (("head", eval prelude (EVar "head")):((binder, eval prelude el):env_old)) body

--- >>> eval (("head", eval prelude (EVar "head")):(((VCons head tail), eval prelude el):env_old)) (eval [] el)
--- >>> 1
-- Variable not in scope: el :: Expr
-- Variable not in scope: env_old :: [(Value, Value)]
-- Variable not in scope: el :: Expr

--- >>> eval prelude (EApp (EVar "head") el)
-- Variable not in scope: el :: Expr
--- >>> eval prelude el
-- Variable not in scope: el :: Expr

-- >>> let el = EBin Cons (EInt 1) ENil
--- >>> let ea = EApp (EVar "head") el
--- >>> eval prelude ea
-- Error {errMsg = "unbound variable: head"}

--- >>> eval prelude (EVar "head")
-- Error {errMsg = "unbound variable: head"}

-- where
  --         Env     Id   Expr          Env           Id
--   VClos env_old binder body = eval prelude (EVar "head")
-- prelude has ("head", VClos env_some bindersome bodysome)



-- eval prelude (EBin Cons(EInt 2) ENil) = evalOp Cons (eval prelude (EInt 2)) (eval prelude ENil) = 

-- >>> execExpr ENil 
-- []

-- >>> execExpr (EBin Cons (EInt 2) ENil)
-- (2 : [])

--- >>> eval (("head", VClos env_old body):((binder, VCons (VInt 1) VNil)):env_old) body
--- >>> where VClos env_old binder body = eval prelude (EVar "head")
---                          []    ENil
---                     []  x:xs     x                     Expr   Expr
---                         "xs"   EIf (Eq (EVar "xs") ENil) (ENil) (ELet (Evar "xs") EBin Cons (EVar "xhead") (EVar "xtail")


--- >>> let ex = EBin Cons (EInt 2) (EBin Cons (EInt 1) (EBin Cons (EInt 3) ENil))
--- >>> eval [] ex
-- (2 : (1 : (3 : [])))



-- >>> VPrim ((VCons (VInt n) VNil) -> (VInt n))
-- Pattern syntax in expression context:
--     (VCons (VInt n) VNil) -> (VInt n)
-- >>> VPrim (VCons (VInt head) VCons(VNil)) -> (VInt head))
-- parse error on input ‘->’

-- >>> let list2 = VCons (VInt 1) (VCons (VInt 2) (VCons (VInt 3) VNil))
-- >>> prim = VClos [] "n" VPrim (VCons (eval [] (EVar "n")) tail -> (eval [] (EVar "n")))
-- >>> eval ["head",prim] "head"
-- Pattern syntax in expression context:
--     VCons (eval [] (EVar "n")) tail -> (eval [] (EVar "n"))

--- >>> let elet = ELet "xs" ex () 

-- >>> (eval [("v", VNil)] (EVar "v")) == VNil
-- True




-- >>> eval env0 (EApp (EVar "f") (EInt 2)) 
-- Error {errMsg = "unbound variable: f"}

-- eval fun to a VFun
-- eval arg to a Value
-- substitute argVal for the function parameter
--------------------------------------------------------------------------------
evalOp :: Binop -> Value -> Value -> Value
--------------------------------------------------------------------------------
evalOp Plus (VInt v1) (VInt v2) = VInt (v1 + v2)
evalOp Minus (VInt v1) (VInt v2) = VInt (v1 - v2)
evalOp Mul (VInt v1) (VInt v2) = VInt (v1 * v2)
evalOp And (VBool b1) (VBool b2) = VBool (b1 && b2)
evalOp Or (VBool b1) (VBool b2) = VBool (b1 || b2)
evalOp Eq (VInt i1) (VInt i2) = VBool (i1 == i2)
evalOp Eq (VBool b1) (VBool b2) = VBool (b1 == b2)
evalOp Ne (VInt i1) (VInt i2) = VBool (i1 /= i2)
evalOp Ne (VBool b1) (VBool b2) = VBool (b1 /= b2)
evalOp Lt (VInt v1) (VInt v2) = VBool (v1 < v2)
evalOp Le (VInt v1) (VInt v2) = VBool (v1 <= v2)
evalOp Cons (VInt v1) VNil = VCons (VInt v1) VNil
evalOp Cons (VInt v1) (VCons v2 v3) = VCons (VInt v1) (VCons v2 v3)
evalOp _ _ _ = throw (Error "type error: binop")

--------------------------------------------------------------------------------
-- | `lookupId x env` returns the most recent
--   binding for the variable `x` (i.e. the first
--   from the left) in the list representing the
--   environment, and throws an `Error` otherwise.
--
-- >>> lookupId "z1" env0
-- 0
-- 
-- >>> lookupId "x" env0
-- 1
-- 
-- >>> lookupId "y" env0
-- 2
-- 
-- >>> lookupId "mickey" env0
-- Error {errMsg = "unbound variable: mickey"}
--------------------------------------------------------------------------------
lookupId :: Id -> Env -> Value
--------------------------------------------------------------------------------
lookupId x [] = throw (Error ("unbound variable: " ++ x))
lookupId x (e:es)
  | x == bind = body
  | otherwise = lookupId x es
    where
      (bind, body) = e

prelude :: Env
prelude =
  [ -- HINT: you may extend this "built-in" environment
    -- with some "operators" that you find useful...
    ("head",VClos [( "prim" ,VPrim (\(VCons (VInt head) _) -> VInt head))] "xs" (EApp (EVar "prim") (EVar "xs"))),
    ("tail",VClos [( "prim" ,VPrim (\(VCons (VInt _) tail) -> tail))]      "xs" (EApp (EVar "prim") (EVar "xs")))
  ]

-- >>> let el = EBin Cons (EInt 1) (EBin Cons (EInt 2) ENil)
-- >>> execExpr el
-- >>> VClos env_old binder body = eval prelude (EVar "head") -- eval prelude (EApp (EVar "head") el) -- return (eval prelude (EApp (EVar "head") el))-- execExpr (EApp (EVar "head") el)
-- >>> env_old
-- >>> binder
-- >>> body
-- >>> return (eval prelude (EApp (EVar "head") el))-- eval prelude (EApp (EVar "head") el)-- eval (("head", eval prelude (EVar "head")):(("xs", eval prelude el):env_old)) ((EVar "prim") (EVar "xs"))
-- parse error (possibly incorrect indentation or mismatched brackets)




--- >>> let vph = VPrim (\(VCons (VInt head) _) -> VInt head)
--- >>> let el = EBin Cons (EInt 2) (EBin Cons (EInt 3) ENil)
--- >>> let vl = eval prelude el
--- >>> let ep = EVar "prim"
-- >>> VPrim vp = eval [( "prim" ,vph)] ep
-- >>> vp vl
-- 2

--- >>> let vpt = VPrim (\(VCons (VInt _) tail) -> tail)
--- >>> let xs = VCons (VInt 2) (VCons (VInt 3) VNil)
--- >>> let ep = EVar "prim"
-- >>> VPrim vp = eval [( "prim" ,vpt)] ep
-- >>> vp xs
-- (3 : [])





--  >>> eval [( "prim" ,ve), ("xs", xs)] (EApp ep ex)

--  >>> eval (("prim", eval [( "prim" ,ve), ("xs", xs)] (EVar "prim")): ((binder, eval [( "prim" ,ve), ("xs", xs)] es):env_old)) body

-- 
-- C:Usersbeomsu_samsungDocumentsGitHub04-nano-bkim9srcLanguageNanoEval.hs:216:6-50: Non-exhaustive patterns in VClos env_old
--                                                                                        binder body



--- >>> let ve = \(VCons (VInt head) tail) -> (VInt head) 
--- >>> VPrim ve
--  >>> ve (VCons (VInt 2) VNil)
-- <<primitive-function>>
-- 2

-- >>> (\(VCons (VInt head) tail) -> (tail)) (VCons (VInt 2) (VCons (VInt 3) VNil))
-- (3 : [])


env0 :: Env
env0 =  [ ("z1", VInt 0)
        , ("x" , VInt 1)
        , ("y" , VInt 2)
        , ("z" , VInt 3)
        , ("z1", VInt 4)
        ]

--------------------------------------------------------------------------------
