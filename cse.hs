{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

import GHC.Generics (Generic)

import Data.Hashable (Hashable)
import Data.Monoid ((<>))
import Data.Char (isLetter)

import qualified Data.Attoparsec.ByteString.Char8 as AT
import qualified Data.ByteString.Char8            as BS
import qualified Data.ByteString.Builder          as BD
import qualified Data.HashMap.Lazy                as HM

import Control.Applicative (Applicative(..),Alternative(..),(<$>))
import Control.Monad (replicateM)

import System.IO (stdout)

-- Expressions --

data Expr = Var Name
          | Sub Ref
          | App Name Expr Expr
          deriving (Eq,Generic)
type Ref  = Int
type Name = BS.ByteString

instance Hashable Expr

-- Builder --

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

name :: AT.Parser BS.ByteString
name = AT.takeWhile isLetter

comma, open, close :: AT.Parser Char
comma = AT.char ','
open  = AT.char '('
close = AT.char ')'

expr :: AT.Parser Expr
expr  =  App <$> name <* open <*> expr <* comma <*> expr <* close
     <|> Var <$> name

-- Elimination --

type State = (Ref, HM.HashMap Expr Ref)

mkState :: State
mkState = (1, HM.empty)

cse :: State -> Expr -> (State,Expr)
cse st@(i,m) e = case HM.lookup e m of
  Just i'  -> (st, Sub i')
  Nothing -> case e of
    App n l r -> (str, App n l' r')
      where (stl,l') = cse st' l
            (str,r') = cse stl r
    Var _     -> (st', e)
    Sub _     -> error "this can't happen"
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

