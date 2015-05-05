module Expression.Parser where

import Expression (Expression(E,L), SLeaf, makeAtom, makeNumber,  SExpression)
import Expression.Pattern (Pattern, PLeaf(P,S))

import Data.Text

import Text.Parsec hiding ((<|>), many, optional)
import Text.Parsec.Text

import Control.Applicative

expr :: Parser a -> Parser (Expression a)
expr p =  L <$> between (tok (char '(')) (tok (char ')')) (many (expr p))
      <|> E <$> tok p

tok :: Parser a -> Parser a
tok p = p <* spaces

identifier :: Parser String
identifier = (:) <$> letter <*> many alphaNum

atom :: Parser String
atom = tok identifier

negNumberLit :: Parser String
negNumberLit = (:) <$> char '-' <*> many1 digit

numberLit :: Parser String
numberLit = many1 digit

number :: Parser Integer
number = read <$> (tok numberLit <|> tok negNumberLit)

sleaf :: Parser SLeaf
sleaf = (makeAtom <$> atom) <|> (makeNumber <$> number)

sexpr :: Parser SExpression
sexpr = expr sleaf

namedPattern :: Parser String
namedPattern = identifier <* char '_' <* notFollowedBy (char '_')

unnamedPattern :: Parser String
unnamedPattern = (const "" <$> char '_') <* notFollowedBy (char '_')

pattern :: Parser String
pattern = tok (namedPattern <|> unnamedPattern)

pleaf :: Parser (PLeaf SLeaf)
pleaf = try (P . makeAtom <$> pattern) <|> (S <$> sleaf)

pexpr :: Parser (Pattern SLeaf)
pexpr = expr pleaf

parseSExpression :: String -> Either ParseError SExpression
parseSExpression = parse sexpr "(unknown)" . pack

parseSPattern :: String -> Either ParseError (Pattern SLeaf)
parseSPattern = parse pexpr "(unknown)" . pack
