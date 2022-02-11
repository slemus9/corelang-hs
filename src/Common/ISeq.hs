module Common.ISeq where

-- Abstract data type for pretty-printing
-- What operations do we want to perform? / What is the efficient way of implementing them?
class ISeq s where
  iNil :: s
  iStr :: String -> s
  iAppend :: s -> s -> s
  iDisplay :: s -> String
  iNewLine :: s
  iIndent :: s -> s
  iConcat :: [s] -> s
  iInterleave :: s -> [s] -> s
  iNum :: Int -> s
  iFWNum :: Int -> Int -> s
  iLayn :: [s] -> s

data ISeqImpl = INil
  | IStr String 
  | IAppend ISeqImpl ISeqImpl
  | IIndent ISeqImpl
  | INewLine
  deriving Show

{-
  Flatten that takes into account indentation.

  flatten :: Int (Current column, 0-based index)
              -> [(ISeq, Int)] (Work list, tuple with an ISeq and it's current indent)
              -> String (Result)
   
-}
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

space :: Int -> String
space n = replicate n ' '

instance ISeq ISeqImpl where

  -- iNil :: ISeq
  iNil = INil 

  -- iStr :: String -> ISeq
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

  -- Append two ISeq's
  -- iAppend :: ISeq -> ISeq -> ISeq
  iAppend INil seq = seq
  iAppend seq INil = seq
  iAppend seq1 seq2 = IAppend seq1 seq2

  -- iDisplay :: ISeq -> String
  iDisplay seq = flatten 0 [(seq, 0)]

  -- New Line followed by a number of spaces determined by the current indentation
  -- iNewLine :: ISeq
  iNewLine = INewLine 

  -- Indents an ISeq to line up with the current column
  -- iIndent :: ISeq -> ISeq
  iIndent = IIndent 

  -- iConcat :: [ISeq] -> ISeq
  iConcat = foldr iAppend iNil 

  -- Interleave ISeq's with a separator (which is also an ISeq)
  -- Join each pair of elements with separator
  -- iInterleave :: ISeq -> [ISeq] -> ISeq
  iInterleave _ [] = iNil
  iInterleave sep (seq : seqs) = 
    seq `iAppend` iConcat (prependToAll seqs) 
    where
      prependToAll = map (sep `iAppend`)

  -- iNum :: Int -> ISeq
  iNum = iStr . show

  -- iFWNum :: Int -> Int -> ISeq
  -- same as iNum but the result is left-padded with spaces to an specified width
  iFWNum width n = 
    iStr $ space (width - length digits) ++ digits
    where
      digits = show n 

  -- iLayn :: [ISeq] -> ISeq
  -- function to enumerate each ISeq, and to separate each one by a new line
  iLayn seqs =
    iConcat $ zipWith layItem [1..] seqs
    where
      layItem n seq =
        iConcat [iFWNum 4 n, iStr ")", iIndent seq, iNewLine]