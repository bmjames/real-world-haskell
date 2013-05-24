module PrettyStub where

import SimpleJSON

data Doc = ToBeDefined
           deriving (Show)

string :: String -> Doc
string = undefined

text :: String -> Doc
text = undefined

double :: Double -> Doc
double = undefined


enclose :: Char -> Char -> Doc -> Doc
enclose l r x = char l <> x <> char r

(<>) :: Doc -> Doc -> Doc
a <> b = undefined

char :: Char -> Doc
char = undefined

hcat :: [Doc] -> Doc
hcat = undefined

