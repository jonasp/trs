module Main where

import Expression
import Expression.Parser

import System.Console.ANSI

Right exp = parseSExpression "(a b (Plus 1 2) (d))"

{-main :: IO ()-}
{-main = do-}
    
