module PandocMario (
    behead
  , withPandoc
  , simpleSearch
) where

import Text.Pandoc.JSON
import Data.Aeson (eitherDecode)
import Data.List (intersperse)
import Data.Maybe (catMaybes)
import System.Exit (exitFailure)
import qualified Data.ByteString.Lazy as BL

-- | An example filter from <http://pandoc.org/filters.html>
behead :: Block -> Block
behead (Header n _ xs) | n >= 2 = Para [Emph xs]
behead x = x

-- | Run an operation on a Pandoc object
withPandoc :: (a -> IO ()) -> (Pandoc -> a) -> IO ()
withPandoc f g = BL.getContents >>=
  f . g . either error id . eitherDecode

-- | A simple search
simpleSearch :: String -> IO ()
simpleSearch s = withPandoc matchOrDie (paraMatchs (string2block s))

paraMatchs :: Block -> Pandoc -> [String]
paraMatchs b (Pandoc _ bs) = catMaybes $ map (paraMatch b) bs

paraMatch :: Block -> Block -> Maybe String
paraMatch (Para xs) (Para ys) =
  if
    infixMatch xs ys
  then
    Just (asString ys)
  else
    Nothing
paraMatch _ _ = Nothing

asString :: [Inline] -> String
asString = concat . map writeInline

writeInline :: Inline -> String
writeInline (Str s) = s
writeInline (Emph xs)        = concat . map writeInline $ xs
writeInline (Strong xs)      = concat . map writeInline $ xs
writeInline (Strikeout xs)   = concat . map writeInline $ xs
writeInline (Superscript xs) = concat . map writeInline $ xs
writeInline (Subscript	 xs) = concat . map writeInline $ xs
writeInline (SmallCaps	 xs) = concat . map writeInline $ xs
writeInline (Quoted SingleQuote xs) = concat . map writeInline $ xs
writeInline (Quoted DoubleQuote xs) = concat . map writeInline $ xs
writeInline Space     = " "
writeInline SoftBreak = " "
writeInline LineBreak = " "
writeInline _ = ""
-- Cite [Citation] [Inline]   | Citation (list of inlines)
-- Code Attr String           | Inline code (literal)
-- Math MathType String       | TeX math (literal)
-- RawInline Format String    | Raw inline
-- Link Attr [Inline] Target  | Hyperlink: alt text (list of inlines), target
-- Image Attr [Inline] Target | Image: alt text (list of inlines), target
-- Note [Block]               | Footnote or endnote
-- Span Attr [Inline]         | Generic inline container with attributes


infixMatch :: [Inline] -> [Inline] -> Bool
infixMatch [] _ = True
infixMatch _ [] = False
infixMatch (x:xs) (y:ys) = (x == y && infixMatch xs ys) || infixMatch (x:xs) ys

-- | Either print matching paragraphs and exit with SUCCESS, or exit with FAIL
matchOrDie :: [String] -> IO ()
matchOrDie [] = exitFailure
matchOrDie xs = mapM_ putStrLn xs

string2block :: String -> Block
string2block = Para . intersperse Space . map Str . words
