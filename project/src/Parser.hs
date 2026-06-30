module Parser
  ( parseProgram,
  )
where

import AST
import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens, brackets, braces :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
brackets = between (symbol "[") (symbol "]")
braces = between (symbol "{") (symbol "}")

reserved :: [String]
reserved = ["let", "if", "then", "else", "observe", "return", "true", "false"]

identifier :: Parser String
identifier = lexeme . try $ do
  name <- (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_')
  if name `elem` reserved
    then fail $ show name ++ " is a reserved keyword"
    else return name

-- try float first, otherwise we'd always parse "0.5" as integer 0
numLit :: Parser Value
numLit =
  lexeme $
    try (RealV <$> L.float)
      <|> (IntV . fromIntegral <$> (L.decimal :: Parser Integer))

boolLit :: Parser Value
boolLit = (BoolV True <$ symbol "true") <|> (BoolV False <$ symbol "false")

atom :: Parser Expr
atom =
  (Lit <$> try numLit)
    <|> (Lit <$> boolLit)
    <|> (Var <$> identifier)
    <|> parens expr

expr :: Parser Expr
expr = makeExprParser atom operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [infixL "*" Mul, infixL "/" Div],
    [infixL "+" Add, infixL "-" Sub],
    [infixL "==" Eq, infixL "<" Lt],
    [infixL "&&" And],
    [infixL "||" Or]
  ]
  where
    infixL name op = InfixL (BinOp op <$ symbol name)

distribution :: Parser Distribution
distribution =
  Distribution
    <$> identifier
    <*> parens (expr `sepBy` symbol ",")

sampleStmt :: Parser Statement
sampleStmt =
  Sample
    <$> try (identifier <* symbol "~")
    <*> distribution
    <* symbol ";"

letStmt :: Parser Statement
letStmt =
  Let
    <$> (symbol "let" *> identifier)
    <*> (symbol "=" *> expr)
    <* symbol ";"

observeStmt :: Parser Statement
observeStmt =
  Observe
    <$> (symbol "observe" *> identifier)
    <*> (symbol "=" *> expr)
    <* symbol ";"

ifStmt :: Parser Statement
ifStmt =
  If
    <$> (symbol "if" *> expr)
    <*> (symbol "then" *> braces (many statement))
    <*> (symbol "else" *> braces (many statement))

returnStmt :: Parser Statement
returnStmt =
  Return
    <$> (symbol "return" *> brackets (identifier `sepBy` symbol ","))
    <* symbol ";"

statement :: Parser Statement
statement =
  sampleStmt
    <|> letStmt
    <|> observeStmt
    <|> ifStmt
    <|> returnStmt

program :: Parser Program
program = sc *> (Program <$> many statement) <* eof

parseProgram :: String -> Either (ParseErrorBundle String Void) Program
parseProgram = parse program "<input>"
