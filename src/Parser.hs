{-# LANGUAGE OverloadedStrings #-}
module Parser where

import           Control.Monad.Combinators.Expr
import           Control.Monad.State
import qualified Data.Map                       as M
import           Data.Text                      (Text)
import           Data.Void                      (Void)
import           Text.Megaparsec                hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

import Formula

type Parser = ParsecT Void Text (State (M.Map String Int))

sc :: Parser ()
sc = L.space space1
  (L.skipLineComment "%")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

pVariable :: Parser Formula
pVariable = do
  name <- lexeme (some (alphaNumChar <|> char '_') <?> "variable")
  vars <- get
  -- Check if variable is in the state
  let idx = M.findWithDefault (M.size vars) name vars
  -- Update state with new variable
  put (M.insert name idx vars)
  return (Var idx)

pConstant :: Parser Formula
pConstant = choice
  [ Bot <$ symbol "$false"
  , Top <$ symbol "$true"
  ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pUnitaryFormula :: Parser Formula
pUnitaryFormula = choice
  [ parens pLogicFormula
  , pConstant
  , pVariable
  ]

prefix :: Text -> (Formula -> Formula) -> Operator Parser Formula
prefix name f = Prefix (foldr1 (.) <$> some (f <$ symbol name))

binary :: Text -> (Formula -> Formula -> Formula) -> Operator Parser Formula
binary name f = InfixR (f <$ symbol name)

operatorTable :: [[Operator Parser Formula]]
operatorTable =
  [ [ prefix "~" neg ]
  , [ binary "&" (:&) ]
  , [ binary "|" (:|) ]
  , [ binary "=>" (:>) ]
  , [ binary "<=>" (<:>) ]
  ]

pLogicFormula :: Parser Formula
pLogicFormula = makeExprParser pUnitaryFormula operatorTable

-- | TPTP Syntax based parser: https://tptp.org/TPTP/SyntaxBNF.html
parse :: String -> Text -> Formula
parse file input
  | result <- evalState (runParserT (sc *> pLogicFormula <* eof) file input) M.empty
  = case result of
  Left  e -> error $ errorBundlePretty e
  Right f -> f
