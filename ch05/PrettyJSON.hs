module PrettyJSON (renderJValue) where

import Numeric (showHex)
import Data.Bits (shiftR, (.&.))
import Data.Char (ord)
import Data.List (intercalate)

import SimpleJSON
import Prettify

renderJValue :: JValue -> Doc

renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull         = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str

renderJValue (JObject o) = series '{' '}' field o
  where field (name,val) = string name
                           <> text ": "
                           <> renderJValue val

renderJValue (JArray a) = series '[' ']' renderJValue a

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
  Just r                 -> text r
  Nothing | mustEscape c -> hexEscape c
          | otherwise    -> char c
    where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'
          
simpleEscapes = let ch a b = (a, ['\\',b])
                in zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise   = astral (d - 0x10000)
  where d = ord c

smallHex :: Int -> Doc
smallHex x = text "\\u"
             <> text (replicate (4 - length h) '0')
             <> text h
  where h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
  where a = (n `shiftR` 10) .&. 0x3ff
        b = n .&. 0x3ff

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close
                       . fsep . punctuate (char ',') . map item

