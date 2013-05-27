module Prettify where

import Data.List (intersperse)
import Data.Monoid hiding ((<>))

import SimpleJSON

data Doc = Empty
         | Char Char
         | Text String
         | Line 
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show)

empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s  = Text s

double :: Double -> Doc
double = text . show

line :: Doc
line = Line

enclose :: Char -> Char -> Doc -> Doc
enclose l r x = char l <> x <> char r

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = []
punctuate p [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

(<>) :: Doc -> Doc -> Doc
Empty <> y     = y
x     <> Empty = x
x     <> y     = Concat x y

hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = Union (flatten x) x

flatten :: Doc -> Doc
flatten (Concat x y) = Concat (flatten x) (flatten y)
flatten Line         = Char ' '
flatten (Union x _)  = flatten x
flatten x            = x

compact :: Doc -> String
compact x = transform [x]
  where transform []     = ""
        transform (d:ds) = case d of
          Empty      -> transform ds
          Char c     -> c : transform ds
          Text s     -> s ++ transform ds
          Line       -> '\n' : transform ds
          Concat a b -> transform (a:b:ds)
          Union _ b  -> transform (b:ds)
          
pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
  where best col (d:ds) =
          case d of
            Empty      -> best col ds
            Char c     -> c : best (col + 1) ds
            Text s     -> s ++ best (col + length s) ds
            Line       -> '\n' : best 0 ds
            Concat a b -> best col (a:b:ds)
            Union a b  -> nicest col (best col (a:ds)) (best col (b:ds))
        best _ _ = ""
        nicest col a b | (width - least) `fits` a = a
                       | otherwise = b
                       where least = min width col

fits :: Int -> String -> Bool
fits w _ | w < 0 = False
fits w ""        = True
fits w ('\n':_)  = True
fits w (c:cs)    = fits (w - 1) cs

-- my own version of nest, not exactly as specified in the exercise
nest :: Int -> Doc -> Doc
nest spaces doc = nest' 0 [doc]
  where nest' n (d:ds) = case d of
          Empty           -> nest' n ds
          Char c
            | isOpening c -> d <> nest' (n + 1) (line:ds)
            | isClosing c -> d <> nest' (n - 1) ds
            | otherwise   -> d <> nest' n ds
          Text s          -> d <> nest' n ds
          Line            -> d <> indent n spaces <> nest' n ds
          Concat a b      -> nest' n (a:b:ds)
          Union _ b       -> nest' n (b:ds) 
        nest' _ []      = empty

isOpening c = c == '{' || c == '[' 
isClosing c = c == '}' || c == ']'
indent n spaces = Text $ replicate (n * spaces) ' '

instance Monoid Doc where
  mempty  = empty
  mappend = (<>)
