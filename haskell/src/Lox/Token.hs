module Lox.Token where

import Relude
import Prelude (Show (show))

data SingleCharToken
  = LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | SLASH
  | STAR
  deriving (Show, Eq)

data MultiCharToken
  = BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL
  deriving (Show, Eq)

data LitToken
  = IDENTIFIER
  | STRING
  | NUMBER
  deriving (Show, Eq)

data KeywordToken
  = AND
  | CLASS
  | ELSE
  | FALSE
  | FUN
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE
  deriving (Show, Eq)

data BaseToken = Single SingleCharToken | Multi MultiCharToken | Lit LitToken | Key KeywordToken | EOF deriving (Show, Eq)

-- tokentype, lexeme, literal, line
data Token = Token BaseToken Text (Maybe Text) Int deriving (Eq)

instance Show Token where
  show (Token tokenType lexeme literal _) = toString $ (toText . Prelude.show) tokenType <> " " <> lexeme <> " " <> toText (Prelude.show literal)
