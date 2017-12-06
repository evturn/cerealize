module PrettyJSON where

import           Cereal
import           Data.List (intercalate)

data Doc = ToBeDefined
  deriving Show

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

enclose :: Char -> Char -> Doc -> Doc
enclose l r x = char l <> x <> char r

(<>) :: Doc -> Doc -> Doc
a <> b = undefined

char :: Char -> Doc
char c = undefined

hcat :: [Doc] -> Doc
hcat xs = undefined

text :: String -> Doc
text s = undefined

double :: Double -> Doc
double n = undefined

renderJValue :: JValue -> Doc
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull         = text "null"
renderJValue (JNumber n)   = double n
renderJValue (JString s)   = string s
renderJValue (JObject o) = "{" ++ pairs o ++ "}"
  where
    pairs [] = ""
    pairs ps = intercalate ", " (map renderPair ps)
    renderPair (k, v) = show k ++ ": " renderJValue v
renderJValue (JArray a) = "[" ++ values a ++ "]"
  where
    values [] = ""
    values vs = intercalate ", " (map renderJValue vs)

putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)
