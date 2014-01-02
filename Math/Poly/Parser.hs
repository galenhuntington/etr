-- | This limited parser accepts multivariate polynomials using
--   variables @['a'..'z']@ and conditions consisting of two
--   polynomials separated by `<` or `>`, which is translated into
--   a polynomial of the right sign.  Examples of valid expressions:
--
--  @
--    a^2 - b + 3
--    (ab-c)^4 - d(b^3c-(a+9)^2)^2
--  @
--
--  One thing it notably does not support is unary minus; you'll
--  have to write @0-1@ instead of just @-1@.

module Math.Poly.Parser (parseMPoly, parseCond) where

import Prelude ()
import Math.Base

import Data.Maybe

import Math.Poly.Multi
import Text.ParserCombinators.Parsec

-- Simple combinators.
parens = between (char '(') (char ')')
whitespace = skipMany (oneOf " \t\r")

-- | Parse a multivariate polynomial.
parseMPoly :: GenParser Char st (MPoly Integer)
parseMPoly = do whitespace; x <- whl; return x where
  whl = do p <- chainl1 mt (wh (oneOf "+-" >>= \x -> return $ if x=='+' then (+) else (-))); return p
  mt = chainl1 et (do optional (wh (char '*')); return (*))
  et = do r <- wh bot; e <- option 1 (do wh $ char '^'; num); return (r^fromInteger e)
  bot = parens whl <|> (num >>= return . unit) <|> ltr
  num = wh (many1 digit) >>= (return . read)
  ltr = do l <- lower; return $ x_ (fromEnum l - 97)
  wh x = do y <- x; whitespace; return y

-- | Parse a condition.
parseCond :: String -> Either ParseError (MPoly Integer, ())
parseCond = parse me "" where
  me = do
    s1 <- parseMPoly
    sgn <- oneOf "<>"
    s2 <- parseMPoly
    eof
    return $ (if sgn == '<' then s2-s1 else s1-s2, ())
