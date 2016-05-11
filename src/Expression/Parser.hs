module Expression.Parser where

import Expression (Expression(E,L), SLeaf, makeAtom, makeNumber,  SExpression)
import Expression.Pattern (Pattern, PLeaf(P,PS,PNS,S))

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

patternSymbol :: Parser Char
patternSymbol = char '_' <* notFollowedBy (char '_')

patternSeqSymbol :: Parser Char
patternSeqSymbol = char '_' <* char '_' <* notFollowedBy (char '_')

patternNullSeqSymbol :: Parser Char
patternNullSeqSymbol = char '_' <* char '_' <* char '_' <* notFollowedBy (char '_')

namedPattern :: Parser (PLeaf SLeaf)
namedPattern = P . makeAtom <$> tok (identifier <* patternSymbol)

namedPatternSeq :: Parser (PLeaf SLeaf)
namedPatternSeq = PS . makeAtom <$> tok (identifier <* patternSeqSymbol)

namedPatternNullSeq :: Parser (PLeaf SLeaf)
namedPatternNullSeq = PNS . makeAtom <$> tok (identifier <* patternNullSeqSymbol)

unnamedPattern :: Parser (PLeaf SLeaf)
unnamedPattern = P . makeAtom <$> tok (const "" <$> patternSymbol)

unnamedPatternSeq :: Parser (PLeaf SLeaf)
unnamedPatternSeq = PS . makeAtom <$> tok (const "" <$> patternSeqSymbol)

unnamedPatternNullSeq :: Parser (PLeaf SLeaf)
unnamedPatternNullSeq = PNS . makeAtom <$> tok (const "" <$> patternNullSeqSymbol)

pattern :: Parser (PLeaf SLeaf)
pattern =  try namedPattern
       <|> (try namedPatternSeq)
       <|> (try namedPatternNullSeq)
       <|> (try unnamedPattern)
       <|> (try unnamedPatternSeq)
       <|> unnamedPatternNullSeq

pleaf :: Parser (PLeaf SLeaf)
pleaf = try pattern <|> (S <$> sleaf)

pexpr :: Parser (Pattern SLeaf)
pexpr = expr pleaf

parseSExpression :: String -> Either ParseError SExpression
parseSExpression = parse sexpr "(unknown)" . pack

parseSPattern :: String -> Either ParseError (Pattern SLeaf)
parseSPattern = parse pexpr "(unknown)" . pack
