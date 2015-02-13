{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Char (isLetter)
import Data.Hashable (Hashable(..))
import Data.Monoid ((<>))

import qualified Data.Attoparsec.ByteString.Char8 as AT
import qualified Data.ByteString.Char8            as BS
import qualified Data.ByteString.Builder          as BD
import qualified Data.HashMap.Lazy                as HM

import Control.Applicative (Applicative(..),Alternative(..),(<$>))
import Control.Monad (replicateM)

import System.IO (stdout)

-- Expressions --

data Expr = App Hash Name Expr Expr
          | Var Hash Name
          | Sub Repl
          deriving (Eq)
type Repl = Int
type Name = BS.ByteString
type Hash = Int

instance Hashable Expr where
  hashWithSalt _ e = case e of
    App h _ _ _ -> h
    Var h _     -> h
    Sub _       -> undefined

mkApp :: Name -> Expr -> Expr -> Expr
mkApp n l r = App h n l r
  where h = hash n `hashWithSalt` hash l `hashWithSalt` hash r

mkVar :: Name -> Expr
mkVar n = Var h n
  where h = hash n

-- Builder --

class Buildable a where
  build :: a -> BD.Builder

instance Buildable Expr where
  build e = case e of
    App _ n l r -> BD.byteString n <> "(" <> build l <> "," <> build r <> ")"
    Var _ n     -> BD.byteString n
    Sub r       -> BD.intDec r
   
putBuildLn :: Buildable a => a -> IO ()
putBuildLn a = BD.hPutBuilder stdout $ build a <> BD.word8 10

-- Parser --

name :: AT.Parser BS.ByteString
name = AT.takeWhile isLetter

comma, open, close :: AT.Parser Char
comma = AT.char ','
open  = AT.char '('
close = AT.char ')'

expr :: AT.Parser Expr
expr  =  mkApp <$> name <* open <*> expr <* comma <*> expr <* close
     <|> mkVar <$> name

-- Elimination --

type State = (Repl, HM.HashMap Expr Repl)

mkState :: State
mkState = (1, HM.empty)

cse :: State -> Expr -> (State,Expr)
cse st@(i,m) e = case HM.lookup e m of
  Just i'  -> (st, Sub i')
  Nothing -> case e of
    App h n l r -> (str, App h n l' r') -- leave hash as is!
      where (stl,l') = cse st' l
            (str,r') = cse stl r
    Var _ _   -> (st', e)
    Sub _     -> undefined
    where st' = (i+1, HM.insert e i m)

-- Main --

run :: IO ()
run = do
  l <- BS.getLine
  case AT.parseOnly expr l of
    Left  e -> error e
    Right x -> putBuildLn . snd $ cse mkState x

main :: IO [()]
main = do
  l <- BS.getLine
  let Just (n,_) = BS.readInt l
  replicateM n run

