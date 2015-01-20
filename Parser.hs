{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Prelude hiding (takeWhile)

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char

import Control.Applicative
import Control.Monad

newtype Parser a = Parser { runParser :: ByteString -> Result a }
data    Result a = Done ByteString a
                 | Fail ByteString Error
type    Error    = String

instance Functor Parser where
  f `fmap` Parser p = Parser $ \s -> case p s of
    Done s' a -> Done s' (f a)
    Fail s' e -> Fail s' e

instance Applicative Parser where
  pure a = Parser $ \s -> Done s a

  Parser p <*> Parser q = Parser $ \s -> case p s of
    Done s' f -> case q s' of
      Done s'' a -> Done s'' (f a)
      Fail s'' e -> Fail s'' e
    Fail s' e -> Fail s' e

instance Alternative Parser where
  empty = Parser $ \s -> Fail s ""

  Parser p <|> Parser q = Parser $ \s -> case p s of
    Fail _ _ -> q s -- Backtracking!
    done     -> done

{-
instance Monad Parser where
  return = pure

  -- :: (ByteString -> Result a) -> (a -> ByteString -> Result a) -> ByteString -> Result a
  Parser p >>= f = Parser $ \s -> case p s of
    Done s' a -> case f a of
      Parser q -> q s'
    Fail s' e -> Fail s' e

  fail e = Parser $ \s -> Fail s e
-}

skipWhile :: (Char -> Bool) -> Parser ()
skipWhile t = Parser $ \s -> Done (B.dropWhile t s) () -- Never fails!

takeWhile :: (Char -> Bool) -> Parser ByteString
takeWhile t = Parser $ \s -> case B.span t s of
  (p,s') -> Done s' p -- Never fails!

takeWhile1 :: (Char -> Bool) -> Parser ByteString
takeWhile1 t = Parser $ \s -> case B.span t s of
  (p,s') | B.null p  -> Fail s "takeWhile1: no match"
         | otherwise -> Done s' p
  -- ("",_) -> Fail s "takeWhile1: no match"
  -- (p,s') -> Done s' p

skipTill :: (Char -> Bool) -> Parser ()
skipTill t = skipWhile (not . t) -- Never fails!

takeTill :: (Char -> Bool) -> Parser ByteString
takeTill t = takeWhile (not . t) -- Never fails!

takeTill1 :: (Char -> Bool) -> Parser ByteString
takeTill1 t = takeWhile1 (not . t)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case B.uncons s of
  Just (c',s') | p c'      -> Done s' c'
               | otherwise -> Fail s "char: no match"
  Nothing                  -> Fail s "char: empty string"

char :: Char -> Parser Char
char c = satisfy (== c)

anyChar :: Parser Char
anyChar = satisfy (const True)

string :: ByteString -> Parser ByteString
string p = Parser $ \s -> if p `B.isPrefixOf` s
  then Done (B.drop (B.length p) s) p
  else Fail s "string: no match"

endOfLine :: Parser ()
endOfLine = void (char '\n') <|> void (string "\r\n")

endOfInput :: Parser ()
endOfInput = Parser $ \s -> if B.null s
  then Done s ()
  else Fail s "endOfInput: there is more"
-- endOfInput = Parser $ \s -> case s of
--   "" -> Done s ()
--   _  -> Fail s "endOfInput: there is more"

choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

manyTill :: Parser a -> Parser b -> Parser [a]
manyTill p end = scan
    where scan = (end *> pure []) <|> liftA2 (:) p scan

word :: Parser ByteString
word = takeWhile isLetter

isEndOfLine :: Char -> Bool
isEndOfLine c = c == '\n' || c == '\r'

between :: Char -> Char -> Parser ByteString
between o c = char o *> takeTill (== c) <* char c

parseOnly :: Parser a -> ByteString -> Either Error a
parseOnly p s = case runParser p s of
  Done _ a -> Right a
  Fail _ e -> Left e

