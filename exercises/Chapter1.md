# Chapter 1 - Exercises

## Exercise 1.1

```
For:
  f = EVar "f"
  x = EVar "x"
  pprExpr (mkMultiAp n f x) =
  pprExpr (EAp .. (EAp (EAp f x) x) .. x) 

Time complexity of mkMultiAp n is O(n).
Time complexity of pprExpr (EAp e1 e2) is O(|e1|).
Total time complexity is at least:
O(1 + 2 + .. + n) = O(n * (n + 1) / 2) = O(n^2)
When the initial expressions have a lenght of 1
```

## Exercise 1.2

```haskell
iConcat = foldr iAppend iNil 

iInterleave _ [] = iNil
iInterleave sep (seq : seqs) = 
  seq `iAppend` iConcat (prependToAll seqs) 
  where
    prependToAll = map (sep `iAppend`)
```

## Exercise 1.3

```haskell
pprExpr ELam {args, body} =
  iStr signature `iAppend` pprExpr body
  where
    signature = unwords args ++ ". " 

pprExpr ECase {expr, alts} = 
  iConcat 
    [ header, iNewLine
    , iStr " ", iIndent $ pprAlts alts
    ] 
  where
    header = 
      iConcat [iStr "case ", pprExpr expr, iStr " of"]


pprAExpr :: ISeq s => CoreExpr -> s
pprAExpr e =
  if isAtomicExpr e then pprExpr e
  else iConcat
    [ iStr "("
    , pprExpr e
    , iStr ")"
    ]

pprProgram :: ISeq s => CoreProgram -> s
pprProgram = iInterleave sep . map pprScDefn
  where
    sep = iStr ";\n\n"

pprScDefn :: ISeq s => CoreScDefn -> s
pprScDefn ScDefn {name, args, body} =
  decl `iAppend` iIndent (pprExpr body)
  where
    decl =
      iStr $ name ++ " " ++ unwords args ++ " = "
```

## Exercise 1.4
```
For seq = IAppend (IStr s_1) $ IAppend (IStr s_2) $ .. $ IAppend (IStr s_{m-1}) (IStr s_m)
Let sum_{i=1}^m = n, since s_i ++ s_{i + 1} costs O(|s_i|), when evaluating lazily all the
strings we have a total time complexity of O(n)
```
Example
```haskell
seq = IAppend (IStr "this ") $ IAppend (IStr "is ") $ IAppend (IStr "an ") (IStr "example.")
flatten [seq]
  = flatten [IStr "this ", IAppend (IStr "is ") $ IAppend (IStr "an ") (IStr "example.")]
  = "this " ++ flatten [IAppend (IStr "is ") $ IAppend (IStr "an ") (IStr "example.")]
  (this is already the complete evaluation. (++) is lazy, so will only evaluate the second argument when asked)
  = "this " ++ flatten [IStr "is ", IAppend (IStr "an ") (IStr "example.")]
  = "this " ++ ("is " ++ flatten [IAppend (IStr "an ") (IStr "example.")])
  = "this " ++ ("is " ++ flatten [IStr "an ", IStr "example."])
  = "this " ++ ("is " ++ ("an " ++ flatten [IStr "example."]))
  = "this " ++ ("is " ++ ("an " ++ ("example." ++ "")))
```

## Exercise 1.5

```haskell
iAppend INil seq = seq
iAppend seq INil = seq
iAppend seq1 seq2 = IAppend seq1 seq2
```

## Exercise 1.6
```haskell
flatten :: Int -> [(ISeqImpl, Int)] -> String 
flatten col ((INewLine, indent) : seqs) 
  = '\n' : space indent ++ flatten indent seqs

flatten col ((IIndent seq, indent) : seqs)
  = flatten col $ (seq, col) : seqs

flatten col ((INil, indent) : seqs)
  = flatten col seqs

flatten col ((IStr s, indent) : seqs)
  = s ++ flatten (col + length s) seqs

flatten col ((IAppend seq1 seq2, indent) : seqs)
  = flatten col $ (seq1, indent) : (seq2, indent) : seqs

flatten _ [] 
  = ""
```

## Exercise 1.7

```haskell
-- simple implementation
iStr = iConcat . map withNewLine
  where
    withNewLine c | c == '\n' = INewLine
                  | otherwise = IStr [c]

-- more efficient implementation in both time and space
iStr = iConcat . map (iConcat . withNewLine) . split
  where
  withNewLine [] = []
  withNewLine s
    | head s == '\n' = map (const INewLine) s
    | otherwise = [IStr s]

  split [] = []
  split s = chunk : split rest
    where
    (chunk, rest) = 
      if head s == '\n' then span (== '\n') s
      else span (/= '\n') s
```

## Exercise 1.8

```haskell
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
```

## Exercise 1.9

```haskell
-- ignore comments of the form: || ... \n
clex ('|' : '|' : cs) =
  clex $ tail $ dropWhile (/= '\n') cs
```

## Exercise 1.10

```haskell
-- recognize operators with 2 characters
clex (c1 : c2 : cs) | [c1, c2] `elem` twoCharOps =
  [c1, c2] : clex cs

twoCharOps = ["==", "~=", ">=", "<=", "->"]
```

## Exercise 1.11

```haskell
clex :: Int -> String -> [Token]

-- increase line number 
clex n ('\n' : cs) =
  clex (n + 1) cs

-- ignore whitespace
clex n (c : cs) | isWhiteSpace c = 
  clex n cs

-- ignore comments of the form: || ... \n
clex n ('|' : '|' : cs) =
  clex n $ tail $ dropWhile (/= '\n') cs

-- recognize operators with 2 characters
clex n (c1 : c2 : cs) | [c1, c2] `elem` twoCharOps =
  Token n [c1, c2] : clex n cs

-- recognize numbers as tokens
clex n s @ (c : cs) | isDigit c =
  Token n numToken : clex n rest 
  where
    (numToken, rest) = span isDigit s

-- a letter followed by one or more letters, digits or underscores
clex n s @ (c : cs) | isAlpha c =
  Token n varToken : clex n rest 
  where
    (varToken, rest) = span isCharId s 

-- if none of the above conditions apply, return a token with a single char
clex n (c: cs) = Token n [c] : clex n cs 

clex _ [] = []

isWhiteSpace :: Char -> Bool 
isWhiteSpace c = c `elem` "\t "

data Token = Token
  { lineNum :: Int
  , value   :: String
  }
```

## Exercise 1.12

```haskell
pThen3 :: (a -> b -> c -> d) 
          -> Parser a -> Parser b -> Parser c 
          -> Parser d
pThen3 combine P {parse=p1} P {parse=p2} P {parse=p3}
  = P parse
  where
    parse toks =
      [(combine a b c, toks3) | (a, toks1) <- p1 toks,
                                (b, toks2) <- p2 toks1,
                                (c, toks3) <- p3 toks2]

pThen4 :: (a -> b -> c -> d -> e)
          -> Parser a -> Parser b -> Parser c -> Parser d
          -> Parser e
pThen4 combine P {parse=p1} P {parse=p2} P {parse=p3} P {parse=p4}
  = P parse
  where
    parse toks =
      [(combine a b c d, toks4) | (a, toks1) <- p1 toks,
                                  (b, toks2) <- p2 toks1,
                                  (c, toks3) <- p3 toks2,
                                  (d, toks4) <- p4 toks3]
```

## Exercise 1.13

```haskell
pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = pAlt (pOneOrMore p) (pEmpty [])

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pThen (:) p (pZeroOrMore p)

pEmpty :: a -> Parser a
pEmpty x = P(\toks -> [(x, toks)])
```

## Exercise 1.14

```haskell
pApply :: Parser a -> (a -> b) -> Parser b
pApply P {parse} f = P (map g . parse)
  where
    g (a, rest) = (f a, rest)
```

## Exercise 1.15

```haskell
pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep pSymbol pSep = 
  pThen (:) pSymbol (pZeroOrMore pWithSep)
  where
    pWithSep = pThen (\_ x -> x) pSep pSymbol
```

## Exercise 1.16

```haskell
pSat :: (String -> Bool) -> Parser String
pSat pred = P parse
  where
    parse [] = []
    parse (t : toks) = 
      let s = tokenVal t in [(s, toks) | pred s]

isVar :: String -> Bool
isVar []       = False
isVar (c : _)  = isAlpha c

pVar :: Parser String
pVar = pSat isVar
```

## Exercise 1.17

```haskell
isVar :: String -> Bool
isVar []       = False
isVar s @ (c : _)  = isAlpha c && s `notElem` keywords

pVar :: Parser Token
pVar = pSat isVar
```

## Exercise 1.19

**TODO**

## Exercise 1.20

```haskell
pSc = pThen4 makeSc pName (pZeroOrMore pVar) (pLit "=") pExpr
  where
    pName = pApply pVar tokenVal
    makeSc name args _ body =
      ScDefn name args body
```

## Exercise 1.21

```haskell
pProgram :: Parser CoreProgram 
pProgram = pOneOrMoreWithSep pSc (pLit ";")

pSc = pThen4 makeSc pVar (pZeroOrMore pVar) (pLit "=") pExpr
  where
    makeSc name args _ body =
      ScDefn name args body

pExpr :: Parser CoreExpr
pExpr = foldr1 pAlt parsers
  where
    parsers =
      [ pAExpr
      , pEAp
      , pELet
      , pECase
      , pELam
      ]

pAExpr = foldr1 pAlt parsers
  where
    pEWithParen =
      pThen3  (\_ expr _ -> expr)
              (pLit "(")
              pExpr
              (pLit ")")
    parsers =
      [ pEVar
      , pENum
      , pEConstr
      , pEWithParen
      ]

pEVar = pApply pVar EVar

pENum = pApply pNum ENum

pEConstr = pThen3 (\_ constr _ -> constr) pPack pConstr pClose 
  where
    pPack = pLit "Pack{"
    pClose = pLit "}"
    pConstr = 
      pThen3  (\tag _ arity -> EConstr tag arity)
              pNum
              (pLit ",")
              pNum

pELet = 
  pThen4  (\isRec defs _ body -> ELet isRec defs body)
          pIsRec
          pDefs
          pIn
          pExpr
  where
    pLetOrLetRec = pAlt (pLit "let") (pLit "letrec")
    pIsRec = pApply pLetOrLetRec (== "letrec")
    pIn = pLit "in"
    pDefs = pOneOrMore pEDef

pEDef = 
  pThen3  (\var _ expr -> (var, expr))
          pVar
          (pLit "=")
          pExpr


pECase = pThen ECase pCase pAlters
  where
    pCase =
      pThen3  (\_ expr _ -> expr)
              (pLit "case")
              pExpr
              (pLit "of")

    pAlters = pOneOrMoreWithSep pEAlter (pLit ";")

pEAlter = 
  pThen4  (\tag vars _ expr -> Alter tag vars expr)
          pTag
          pVars
          pArrow
          pExpr
  where
    pTag = 
      pThen3  (\_ tag _ -> tag)
              (pLit "<")
              pNum
              (pLit ">")
    pArrow = pLit "->"
    pVars = pZeroOrMore pVar


pELam = pThen ELam pArgs pExpr
  where
    pSlash = pLit "\\"
    pDot = pLit "."
    pArgs = 
      pThen3  (\_ args _ -> args)
              pSlash
              (pOneOrMore pVar)
              pDot
```

## Exercise 1.22

The alternative starting with <2> should be attached to the inner case. The parser handles this case correctly as the pprint function
shows:

```
f x y = case x of
         <1>  -> case y of
                  <1>  -> 1;
                  <2>  -> 2
```

## Exercise 1.23

```haskell
pEAp = pOneOrMore pAExpr `pApply` mkApChain 
  where
    mkApChain = foldl1 EAp
```

## Exercise 1.24

```haskell
pExpr1 = pInfixOp ["&"] pExpr2 pExpr1

pExpr2 = pInfixOp ["|"] pExpr3 pExpr2

pExpr3 = pInfixOp relops pExpr4 pExpr4 `pAlt` pExpr4

pExpr4 = foldr1 pAlt parsers
  where
    parsers =
      [ pInfixOp ["+"] pExpr5 pExpr4
      , pInfixOp ["-"] pExpr5 pExpr5
      , pExpr5
      ]

pExpr5 = foldr1 pAlt parsers 
  where
    parsers = 
      [ pInfixOp ["*"] pAExpr pExpr5
      , pInfixOp ["/"] pAExpr pAExpr
      , pAExpr
      ]

pInfixOp ops pe1 pe2 = pThen assembleOp pe1 pNextExpr
  where
    pPartial expr op = pThen FoundOp (pLit op) expr
    pNextExpr = 
      foldr (pAlt . pPartial pe2) (pEmpty NoOp) ops
```