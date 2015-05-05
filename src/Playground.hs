module Playground where

import Test.QuickCheck

import Prelude hiding (foldr, exp, sequence)

import Text.Parsec

import Data.Foldable

import Data.Traversable

import Expression
import Expression.Pattern
import Expression.Matching (Rule(Rule), match, bind)
import Expression.Parser

{-import Control.Comonad-}

Right exp = parseSExpression "(a b (Plus 1 2) (d))"
Right pat = parseSPattern "(a x_ y_ (d))"
Right res = parseSExpression "(x (y) 4)"

Right p1 = parseSPattern "(x_ y)"
Right p2 = parseSPattern "y_"

Right exp2 = parseSExpression "(Times (Plus 3 4) 2)"

plusRule = Rule p r where
  Right p = parseSPattern "(Plus a_ b_)"
  Right r = parseSExpression "(Plus b a)"

test = do bs <- match pat exp
          return $ bind bs res

{-wish = do-}
  {-let e = parseSExpression "(a b (Plus 1 2) (d))"-}
      {-r = parseSRule       "(a x_ y_        (d)) :> (x y)"-}
      {-rewriteWith r e-}
      

prop_parse_show :: SExpression -> Bool
prop_parse_show e = c (Right e) (parseSExpression (show e))
  where
    c (Right a) (Right b) = a == b
    c _         _         = False

{-instance Comonad Expression where-}
  {-duplicate e = E e-}
  {-extract (E e) = a-}
  {-extract (L es) = -}
