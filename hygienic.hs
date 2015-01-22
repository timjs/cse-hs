{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

import GHC.Generics (Generic)

-- import Data.Attoparsec.ByteString.Char8
import Data.Hashable (Hashable)
import Data.Monoid ((<>))

import qualified Data.ByteString.Char8   as BS
import qualified Data.ByteString.Builder as BD
import qualified Data.HashMap.Lazy       as HM

import Control.Applicative
import Control.Monad

import System.IO (stdout)

import Parser

-- Expressions --

data Expr = Var Name
          | Sub Ref
          | App Name Expr Expr
          deriving (Eq,Generic)
type Ref  = Int
type Name = BS.ByteString

instance Hashable Expr

-- Helpers --

class Buildable a where
  build :: a -> BD.Builder

instance Buildable Expr where
  build e = case e of
    Var n     -> BD.byteString n
    Sub r     -> BD.intDec r
    App n l r -> BD.byteString n <> "(" <> build l <> "," <> build r <> ")"
   
putBuildLn :: Buildable a => a -> IO ()
putBuildLn a = BD.hPutBuilder stdout $ build a <> BD.word8 10

-- Parser --

comma, open, close :: Parser Char
comma = char ','
open  = char '('
close = char ')'

expr :: Parser Expr
expr  =  App <$> word <* open <*> expr <* comma <*> expr <* close
     <|> Var <$> word

-- Elimination --

type State = (Ref, HM.HashMap Expr Ref)

mkState :: State
mkState = (1, HM.empty)

cse :: State -> Expr -> (State,Expr)
cse st@(i,m) e = case HM.lookup e m of
  Just i'  -> (st, Sub i')
  Nothing -> case e of
    App n l r -> (str, App n l' r')
      where st'      = (i+1, HM.insert e i m)
            (stl,l') = cse st' l
            (str,r') = cse stl r
    Var _     -> (st', e)
      where st'      = (i+1, HM.insert e i m)
    Sub _     -> error "this can't happen"

-- Main --

run :: IO ()
run = do
  l <- BS.getLine
  case parseOnly expr l of
    Left  e -> error e
    Right x -> putBuildLn . snd $ cse mkState x

main :: IO [()]
main = do
  l <- BS.getLine
  let Just (n,_) = BS.readInt l
  replicateM n run

