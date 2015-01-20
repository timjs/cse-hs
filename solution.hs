{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

import Prelude hiding (putStrLn, getLine, foldr)
import GHC.Generics (Generic)

import Data.ByteString.Char8 (ByteString, unpack, putStrLn, getLine, readInt)
import Data.ByteString.Builder
import Data.Hashable
import Data.HashMap as HM
import Data.Foldable
import Data.Monoid

import Control.Applicative
import Control.Monad
import Control.Monad.State

import System.IO (stdout)

-- import System.Environment
-- import System.Exit

import Parser

-- Expressions --

data Expr  = Var Name
           | Sub Ref
           | App Name Expr Expr
           deriving Generic

type Ref = Int
type Name = ByteString

instance Hashable Expr

class Buildable a where
  build :: a -> Builder

instance Buildable Expr where
  build e = case e of
    Var n     -> byteString n
    Sub r     -> intDec r
    App n l r -> byteString n <> "(" <> build l <> "," <> build r <> ")"
   
-- instance Foldable Expr where
--   foldr f a e = case e of
--     Var n     -> f n a
--     Sub i     -> f i a
--     App n l r -> foldr f (f n (foldr f a r)) l

-- Helpers --

putBuildLn :: Buildable a => a -> IO ()
putBuildLn a = hPutBuilder stdout $ build a <> word8 10

-- Parser --

comma, open, close :: Parser Char
comma = char ','
open  = char '('
close = char ')'

expr :: Parser Expr
expr  =  App <$> word <* open <*> expr <* comma <*> expr <* close
     <|> Var <$> word

-- Elimination --


-- Main --

run :: IO ()
run = do
  l <- getLine
  case parseOnly expr l of
    Left  e -> error e
    Right x -> putBuildLn x

main :: IO [()]
main = do
  l <- getLine
  let Just (n,_) = readInt l
  replicateM n run

