{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (getLine, putStrLn)

import Data.ByteString.Char8 (ByteString, getLine, readInt)
-- import Data.Char

import Control.Applicative
import Control.Monad

-- import System.Environment
-- import System.Exit

import Parser

-- Tokens and Expressions --

data Expr  = Var Name
           | App Name Expr Expr
           | Sub Node
           deriving (Show,Eq)

type Node = Int
type Name = ByteString

-- Parser --

comma, open, close :: Parser Char
comma = char ','
open  = char '('
close = char ')'

expr :: Parser Expr
expr  =  App <$> word <* open <*> expr <* comma <*> expr <* close
     <|> Var <$> word

-- Main --

run :: IO ()
run = do
  l <- getLine
  case parseOnly expr l of
    Left  e -> error e
    Right x -> print x

main :: IO [()]
main = do
  l <- getLine
  let Just (n,_) = readInt l
  replicateM n run

