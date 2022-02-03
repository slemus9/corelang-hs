{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}

module TemplateInstantiation.Language () where

import TemplateInstantiation.ISeq
import qualified Data.Map as M

data Expr a = EVar Name -- Variables
  | ENum Int -- Numbers
  | EConstr -- Constructors
    { tag   :: Int
    , arity :: Int
    }
  | EAp (Expr a) (Expr a) -- Applications
  | ELet  -- Let(rec) expressions
    { isRec :: Bool 
    , defs  :: [(a, Expr a)]
    , body  :: Expr a
    }
  | ECase -- Case expressions
    { expr :: Expr a
    , alts :: [Alter a]
    }
  | ELam -- Lambda abstractions
    { args :: [a]
    , body :: Expr a
    }
  deriving Show

data Alter a = Alter 
  { tag   :: Int
  , vars  :: [a]
  , expr  :: Expr a
  }
  deriving Show

-- Supercombinator definition
data ScDefn a = ScDefn
  { name :: Name
  , args :: [a]
  , body :: Expr a
  }
  deriving Show

type Name = String
type Program a = [ScDefn a]

type CoreExpr = Expr Name
type CoreAlt = Alter Name
type CoreScDefn = ScDefn Name
type CoreProgram = Program Name


bindersOf :: [(a, b)] -> [a]
bindersOf = map fst

-- Right hand sides of
rhssOf :: [(a, b)] -> [b]
rhssOf = map snd

-- Expressions with no internal structure
isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar _)       = True
isAtomicExpr (ENum _)       = True
isAtomicExpr _              = False

-- Core's small standard prelude
funcExpr :: Name -> CoreExpr -> CoreExpr
funcExpr name = EAp (EVar name)

-- I x = x ;
iDef :: CoreScDefn
iDef = ScDefn 
  { name = "I"
  , args = ["x"]
  , body = EVar "x"
  }

-- K x y = x ;
kDef :: CoreScDefn
kDef = ScDefn 
  { name = "K"
  , args = ["x", "y"]
  , body = EVar "x"
  }

-- K1 x y = y ;
k1Def :: CoreScDefn
k1Def = ScDefn 
  { name = "K1"
  , args = ["x", "y"]
  , body = EVar "y"
  }

-- S f g x = f x (g x) ;
sDef :: CoreScDefn
sDef = ScDefn 
  { name = "S"
  , args = ["f", "g", "x"]
  , body = EAp (f x) (g x)
  } where
    x = EVar "x"
    f = funcExpr "f"
    g = funcExpr "g"

-- compose f g x = f (g x) ;
composeDef :: CoreScDefn
composeDef = ScDefn 
  { name = "compose"
  , args = ["f", "g", "x"]
  , body = f (g x)
  } where
    x = EVar "x"  
    f = funcExpr "f"
    g = funcExpr "g"

-- twice f = compose f f
twiceDef :: CoreScDefn
twiceDef = ScDefn 
  { name = "twice"
  , args = ["f"]
  , body = EAp (comp f) f
  } where 
    f     = EVar "f"
    comp  = funcExpr "compose" 

preludeDefs :: CoreProgram
preludeDefs = 
  [ iDef, kDef, k1Def
  , sDef, composeDef, twiceDef
  ]

-- Pretty-printer

-- alternative foldl definition
-- https://wiki.haskell.org/Foldl_as_foldr
foldll :: (a -> b -> a) -> a -> [b] -> a
foldll f a bs =
   foldr (\b g x -> g (f x b)) id bs a

-- mkMultiAp n e1 e2 = e1 e2 ... e2
--                        (n times)
mkMultiAp :: Int -> Expr a -> Expr a -> Expr a
mkMultiAp n e1 e2 = foldll EAp e1 $ replicate n e2 


-- Binary operations and their precedence
relops = ["==", "~=", ">", ">=", "<", "<="]

binops = M.fromList $
  [ ("|", 1)
  , ("&", 2) 
  ] ++ map (, 3) relops ++
  [ ("+", 4)
  , ("-", 4)
  , ("*", 5)
  , ("/", 5)
  ]

isRecursiveExpr :: Expr a -> Bool 
isRecursiveExpr (EVar _) = False 
isRecursiveExpr (ENum _) = False 
isRecursiveExpr EConstr {} = False 
isRecursiveExpr _ = True 

isAp :: Expr a -> Bool 
isAp (EAp _ _) = True 
isAp _         = False

wrapInParen :: ISeq s => s -> s
wrapInParen body = iConcat [iStr "(", body, iStr ")"]

parenIf :: ISeq s => Bool -> (s -> s)
parenIf pred = if pred then wrapInParen else id

pprExpr :: ISeq s => Int -> CoreExpr -> s
pprExpr _ (EVar v) = iStr v
pprExpr _ (ENum n) = iStr (show n)

{-
  Receives an Int denoting the context precedence.
  If the current binary operation has lower precedence,
  we wrap it in parentheses
-}
pprExpr contextPrec (EAp (EAp (EVar op) e1) e2)
  | Just prec <- M.lookup op binops =
    parenIf (prec < contextPrec) (operation prec)
  where
    operation prec = iConcat 
      [ pprExpr prec e1
      , iStr (" " ++ op ++ " ")
      , pprExpr prec e2
      ]

-- function application
pprExpr p (EAp e1 e2) =
  iConcat 
     [ s1 (pprExpr p e1)
     , iStr " "
     , s2 (pprExpr p e2)
     ]
  where
    s1 = parenIf $ isRecursiveExpr e1 && not (isAp e1)
    s2 = parenIf $ isRecursiveExpr e2

-- let .. in .. expressions
pprExpr p ELet {isRec, defs, body} = 
  iConcat 
    [ iStr keyword, iNewLine
    , iStr "  ", iIndent (pprDefs p defs), iNewLine
    , iStr "in ", pprExpr p body
    ]
  where
    keyword | isRec     = "letrec"
            | otherwise = "let"

-- lambda expressions
pprExpr p ELam {args, body} = 
  iStr signature `iAppend` pprExpr p body
  where
    signature = unwords args ++ ". " 

-- case expressions
pprExpr p ECase {expr, alts} = 
  iConcat 
    [ header, iNewLine
    , iStr " ", iIndent $ pprAlts p alts
    ] 
  where
    header = 
      iConcat [iStr "case ", pprExpr p expr, iStr " of"]

-- constructors
pprExpr _ EConstr {tag, arity} = 
  iStr $ "Pack{" ++ show tag ++ ", " ++ show arity ++ "}"

-- Alternatives
pprAlts :: ISeq s => Int -> [CoreAlt] -> s
pprAlts p = iInterleave (iStr ";\n") . map (pprAlt p)

pprAlt :: ISeq s => Int -> CoreAlt -> s
pprAlt p Alter {tag, vars, expr} = 
  iInterleave (iStr " ")
    [ num
    , variables
    , iStr "->"
    , iIndent $ pprExpr p expr
    ]
  where
    num = iStr $ "<" ++ show tag ++ ">"
    variables = iStr $ unwords vars 
-----------

-- Definitions
pprDefs :: ISeq s => Int -> [(Name, CoreExpr)] -> s
pprDefs p = iInterleave (iStr ";\n") . map (pprDef p) 

pprDef :: ISeq s => Int -> (Name, CoreExpr) -> s
pprDef p (name, value) = 
  iConcat [iStr name, iStr " = ", iIndent (pprExpr p value)]
-----------


-- Supercombinators
pprProgram :: ISeq s => CoreProgram -> s
pprProgram = iInterleave sep . map pprScDefn
  where
    sep = iStr ";\n\n"

pprScDefn :: ISeq s => CoreScDefn -> s
pprScDefn ScDefn {name, args, body} =
  decl `iAppend` iIndent (pprExpr 0 body)
  where
    decl =
      iStr $ name ++ " " ++ unwords args ++ " = "
-----------

pprint :: ISeq s => (CoreProgram -> s) -> CoreProgram -> String
pprint printer = iDisplay . printer

printer :: CoreProgram -> ISeqImpl 
printer = pprProgram

binopExpr op e1 = EAp (EAp (EVar op) e1)
addExpr = binopExpr "+"
subsExpr = binopExpr "-"
multExpr = binopExpr "*"
divExpr = binopExpr "/"

exampleLenExpr = EAp (EVar "length") (EVar "s")
exampleBinopsExpr =
  multExpr exampleLenExpr $ addExpr (EVar "y") (divExpr (EVar "x") (subsExpr (ENum 1) (EVar "z")))
exampleCase = ECase {expr = EVar "b", alts = 
  [ Alter {tag=1, vars=["v"], expr=EVar "\"example1\""}
  , Alter {tag=2, vars=["v1", "v2"], expr=EVar "\"example2\""}
  , Alter {tag=3, vars=["v1", "v2", "v3"], expr=EVar "\"example3\""}
  ]}
exampleDefs = 
  [ ("x", ENum 5)
  , ("y", subsExpr (EVar "x") (ENum 2) )
  , ("z", addExpr (EVar "x") (multExpr (ENum 2) (EVar "y")))
  , ("s", exampleCase)
  , ("n", exampleBinopsExpr)
  ]
exampleExpr = mkMultiAp 9 (EVar "f") (EVar "x")
exampleLet = ELet {isRec = False, defs = exampleDefs, body = exampleExpr}
exampleSc = ScDefn {name = "g", args = ["f", "x", "b"], body = exampleLet}