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
  makeToken n [c1, c2] : clex n cs

-- recognize numbers as tokens
clex n s @ (c : cs) | isDigit c =
  makeToken n numToken : clex n rest 
  where
    (numToken, rest) = span isDigit s

-- a letter followed by one or more letters, digits or underscores
clex n s @ (c : cs) | isAlpha c =
  makeToken n varToken : clex n rest 
  where
    (varToken, rest) = span isCharId s 

-- if none of the above conditions apply, return a token with a single char
clex n (c: cs) = makeToken n [c] : clex n cs 

clex _ [] = []

isWhiteSpace :: Char -> Bool 
isWhiteSpace c = c `elem` "\t "

data Token = Token
  { lineNum :: Int
  , value   :: String
  }

makeToken :: Int -> String -> Token
makeToken n s = Token {lineNum = n, value = s}
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