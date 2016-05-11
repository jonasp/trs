module Main where

import Data.List

import Expression
import Expression.Pattern (Pattern)
import Expression.Matching (Rule(Rule), rewriteAllWith)
import Expression.Parser

import Data.Hashable (Hashable)

unsafeParseSPattern :: String -> Pattern SLeaf
unsafeParseSPattern p = case parseSPattern p of
                          Right p' -> p'
                          Left _   -> error "invalid"

unsafeParseSExpression :: String -> SExpression
unsafeParseSExpression p = case parseSExpression p of
                            Right p' -> p'
                            Left _   -> error "invalid"

makeRule :: String -> String -> Rule SLeaf
makeRule p r = Rule p' r' where
  p' = unsafeParseSPattern p
  r' = unsafeParseSExpression r


compose :: [a -> a] -> a -> a
compose = foldl (flip (.)) id

apply :: (Eq a, Hashable a) => [Rule a] -> Expression a -> Expression a
apply xs = compose (rewriteAllWith <$> xs)

main :: IO ()
main =
  let
    start = unsafeParseSExpression "(Power (Plus a b) 2)"

    squareRule    = makeRule "(Power x_ 2)"  "(Times x x)"
    powerRule     = makeRule "(Times x_ x_)" "(Power x 2)"
    singletonRule = makeRule "(Plus x_)"     "x"
    distRule      =
      makeRule "(Times l___ (Plus x_ y__) r___)" "(Plus (Times l x r) (Times l (Plus y) r))"
    flatRule      = makeRule "(Plus l___ (Plus x__) r___)" "(Plus l x r)"
    comRule       = makeRule "(Plus l___ (Times x_ y_) (Times y_ x_) r___)" "(Plus l (Times 2 x y) r)"

    rewriteSequence = [ squareRule
                      , distRule
                      , singletonRule
                      , distRule
                      , singletonRule
                      , flatRule
                      , flatRule
                      , comRule
                      , powerRule
                      ] in
  mapM_ putStrLn [pretty $ apply rs start | rs <- inits rewriteSequence]

