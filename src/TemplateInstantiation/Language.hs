{-# LANGUAGE DuplicateRecordFields #-}

module TemplateInstantiation.Language where

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