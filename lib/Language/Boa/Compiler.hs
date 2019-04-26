{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

--------------------------------------------------------------------------------
-- | The entry point for the compiler: a function that takes a Text
--   representation of the source and returns a (Text) representation
--   of the assembly-program string representing the compiled version
--------------------------------------------------------------------------------

module Language.Boa.Compiler ( compiler, compile ) where

import           Text.Printf                     (printf)
import           Prelude                 hiding (compare)
import           Control.Arrow           ((>>>))
import           Control.Monad           (void)
import           Data.Maybe
import           Language.Boa.Types      hiding (Tag)
import           Language.Boa.Parser     (parse)
import           Language.Boa.Normalizer (anormal)
import           Language.Boa.Asm        (asm)

--------------------------------------------------------------------------------
compiler :: FilePath -> Text -> Text
--------------------------------------------------------------------------------
compiler f = parse f >>> anormal >>> tag >>> compile >>> asm

-- | to test your compiler with code that is ALREADY in ANF comment out
--   the above definition and instead use the below:

-- compiler f = parse f >>> tag >>> compile >>> asm


--------------------------------------------------------------------------------
-- | The compilation (code generation) works with AST nodes labeled by @Tag@
--------------------------------------------------------------------------------
type Tag   = (SourceSpan, Int)
type AExp  = AnfExpr Tag
type IExp  = ImmExpr Tag
type ABind = Bind    Tag

instance Located Tag where
  sourceSpan = fst

instance Located a => Located (Expr a) where
  sourceSpan = sourceSpan . getLabel

--------------------------------------------------------------------------------
-- | @tag@ annotates each AST node with a distinct Int value
--------------------------------------------------------------------------------
tag :: Bare -> AExp
--------------------------------------------------------------------------------
tag = label

--------------------------------------------------------------------------------
-- | @compile@ a (tagged-ANF) expr into assembly
--------------------------------------------------------------------------------
compile :: AExp -> [Instruction]
--------------------------------------------------------------------------------
compile e = compileEnv emptyEnv e ++ [IRet]


--------------------------------------------------------------------------------
compileEnv :: Env -> AExp -> [Instruction]
--------------------------------------------------------------------------------
compileEnv env v@(Number {})     = [ compileImm env v  ]

compileEnv env v@(Id {})         = [ compileImm env v  ]

compileEnv env e@(Let b e1 e2 l) = error "TBD:compileEnv:Let"

compileEnv env (Prim1 o v l)     = compilePrim1 l env o v

compileEnv env (Prim2 o v1 v2 l) = compilePrim2 l env o v1 v2

compileEnv env (If v e1 e2 l)    = compileEnv env v ++
                                   [ICmp (Reg EAX) (Const 0)] ++
                                   [IJne (BranchTrue l')] ++
                                   compileEnv env e2 ++
                                   [IJmp (BranchDone l')] ++
                                   [ILabel (BranchTrue l')] ++
                                   compileEnv env e1 ++
                                   [ILabel (BranchDone l')]
               where
                    l' = snd(l)

compileImm :: Env -> IExp -> Instruction
compileImm env v = IMov (Reg EAX) (immArg env v)

compileBinds :: Env -> [Instruction] -> [(ABind, AExp)] -> (Env, [Instruction])
compileBinds env is []     = (env, is)
compileBinds env is (b:bs) = compileBinds env' (is ++ is') bs
  where
    (env', is')            = compileBind env b

compileBind :: Env -> (ABind, AExp) -> (Env, [Instruction])
compileBind env (x, e) = (env', is)
  where
    is                 = compileEnv env e
                      ++ [IMov (stackVar i) (Reg EAX)]
    (i, env')          = pushEnv x env

immArg :: Env -> IExp -> Arg
immArg _   (Number n _)  = repr n
immArg env e@(Id x _)    = case (lookupEnv x env) of 
                           Nothing -> err
                           Just num -> stackVar num 
  where
    err                  = abort (errUnboundVar (sourceSpan e) x)

immArg _   e             = panic msg (sourceSpan e)
  where
    msg                  = "Unexpected non-immExpr in immArg: " ++ show (void e)

errUnboundVar :: SourceSpan -> Id -> UserError
errUnboundVar l x = mkError (printf "Unbound variable %s" x) l

--------------------------------------------------------------------------------
-- | Compiling Primitive Operations
--------------------------------------------------------------------------------
compilePrim1 :: Tag -> Env -> Prim1 -> AExp -> [Instruction]
compilePrim1 l env Add1 v = (compileEnv env v) ++ [IAdd (Reg EAX) (Const 1)]
compilePrim1 l env Sub1 v = (compileEnv env v) ++ [ISub (Reg EAX) (Const 1)]

compilePrim2 :: Tag -> Env -> Prim2 -> IExp -> IExp -> [Instruction]
compilePrim2 l env Plus  v1 v2 = error "TBD:compilePrim2:Plus"
compilePrim2 l env Minus v1 v2 = error "TBD:compilePrim2:Minus"
compilePrim2 l env Times v1 v2 = error "TBD:compilePrim2:Times"

--------------------------------------------------------------------------------
-- | Local Variables
--------------------------------------------------------------------------------
stackVar :: Int -> Arg
--------------------------------------------------------------------------------
stackVar i = RegOffset (-4 * i) ESP

--------------------------------------------------------------------------------
-- | Representing Values
--------------------------------------------------------------------------------

class Repr a where
  repr :: a -> Arg

instance Repr Int where
  repr n = Const (fromIntegral n)

instance Repr Integer where
  repr n = Const (fromIntegral n)
