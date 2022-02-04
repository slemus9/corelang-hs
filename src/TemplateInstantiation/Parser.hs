{-# LANGUAGE NamedFieldPuns #-}

module TemplateInstantiation.Parser where

import TemplateInstantiation.Language (CoreProgram)
import Data.Char (isAlpha, isDigit, isAlphaNum)


-- A Token should never be empty
-- type Token = String 
data Token = Token
  { lineNum   :: Int
  , tokenVal  :: String
  }
  deriving Show

twoCharOps = ["==", "~=", ">=", "<=", "->"]

keywords = ["let", "letrec", "case", "in", "of", "Pack"]

makeToken :: Int -> String -> Token
makeToken n s = Token {lineNum = n, tokenVal = s}

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` "\t "

isCharId :: Char -> Bool
isCharId c = isAlphaNum c || c == '_'

{-
  Lexical analysis. Breaks the input into a
  sequence of small chunks called tokens, that
  represent elements such as identifiers, numbers,
  symbols, etc.
-}
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
-----------


-- Parser
newtype Parser a = P { parse :: [Token] -> [(a, [Token])] }

{-
  Tells whether or not the string inside the first token
  has the desired property dictated by the predicate,
  and returns a parser which recognises the token
-}
pSat :: (String -> Bool) -> Parser Token
pSat pred = P parse
  where
    parse [] = []
    parse (t @ Token {lineNum, tokenVal} : toks) = 
      [(t, toks) | pred tokenVal]

{-
  Parse a literal. Takes a Token and returns a parser
  recognizing that particular String of the Token 
-}
pLit :: String -> Parser Token
pLit s = pSat (== s)

{-
  The lexing process ensures that the strings are not empty, but
  also that if the resulting string is starts with a letter, it's
  a variable or a keyword
-}
isVar :: String -> Bool
isVar []       = False
isVar s @ (c : _)  = isAlpha c && s `notElem` keywords

{-
  Decides whether a token is a variable by looking at the first character
-}
pVar :: Parser Token
pVar = pSat isVar

isInt :: String -> Bool 
isInt = all isDigit

pNum :: Parser Int
pNum = pApply (pSat isInt) (read . tokenVal)

{-
  Combines two parsers. parses the same input with the
  two parsers and then joins their result.

  Corresponds to the '|' in BNF grammar
-}
pAlt :: Parser a -> Parser a -> Parser a
pAlt P {parse=p1} P {parse=p2} = P (\toks -> p1 toks ++ p2 toks)

{-
  Combines two parsers in the following way: 
  parses the input with the first parser, then parses the remaining
  tokens with the second parser, and combines their results with a
  function.

  Correspond to the sequencing of symbols in a BNF grammar
-}
pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine P {parse=p1} P {parse=p2} = P parse
  where
    parse toks =
      [(combine a b, toks2) | (a, toks1) <- p1 toks,
                              (b, toks2) <- p2 toks1]

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

{-
  Takes a parser and recognises zero or more occurrences
  of whatever the parser recognises.

  It should either recognise one or more ocurrences or zero ocurrences
-}
pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = pAlt (pOneOrMore p) (pEmpty [])

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pThen (:) p (pZeroOrMore p)

{-
  A parser that always succeeds by leaving its
  input unprocessed, and returning the given value
  as the parsed result
-}
pEmpty :: a -> Parser a
pEmpty x = P(\toks -> [(x, toks)])

{-
  Takes a parser and a function, and returns
  another parser with the function applied to
  its results
-}
pApply :: Parser a -> (a -> b) -> Parser b
pApply P {parse} f = P (map g . parse)
  where
    g (a, rest) = (f a, rest)

{-
  Recognizes one or more occurences of a symbol (first parser),
  separated by another symbol (second parser)
-}
pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep pSymbol pSep = 
  pThen (:) pSymbol (pZeroOrMore pWithSep)
  where
    pWithSep = pThen (\_ x -> x) pSep pSymbol

-----------

{-
  Consumes a sequence of tokens and produces a program
-}
syntax :: [Token] -> CoreProgram
syntax = undefined

-- parse :: String -> CoreProgram 
-- parse = syntax . clex 1

-- Examples
pHelloOrGoodbye :: Parser Token
pHelloOrGoodbye = pAlt (pLit "hello") (pLit "goodbye")

pGreeting :: Parser (Token, Token)
pGreeting =
  pThen3 makeGreeting
    pHelloOrGoodbye
    pVar 
    (pLit "!")
  where
    makeGreeting hg name exclamation = (hg, name)

pGreetings :: Parser [(Token, Token)]
pGreetings = pZeroOrMore pGreeting

dummyTokens = zipWith makeToken [1..]


letOfStr n
  | n <= 0 = []
  | otherwise = "let " ++ alt ++ concat alts ++ ofStr
  where
    alt = "x = y"
    alts = replicate (n - 1) ("; " ++ alt)
    ofStr = "\nof x"

letOfTokens = clex 1 . letOfStr

pLetIn = pThen3 (\_ defs res -> (defs, res)) pLet pDefs pIn 
  where
    pDef = pThen3 (\var _ val -> (var, val)) pVar (pLit "=") pVar
    pDefs = pOneOrMoreWithSep pDef (pLit ";")
    pLet = pLit "let"
    pIn = pThen (\_ b -> b) (pLit "in") pVar 

showPLetInRes (defs, res) = 
  (map showDefs defs, tokenVal res)
  where
    showDefs (var, val) = (tokenVal var, tokenVal val)

-----------