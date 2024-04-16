{-# LANGUAGE OverloadedStrings #-}
module Parser where

import           Control.Monad.Combinators.Expr
import           Control.Monad.State
import           Data.List                      (partition)
import qualified Data.Map                       as M
import           Data.Text                      (Text)
import           Data.Void                      (Void)
import           Text.Megaparsec                hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

import Data.Formula

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
  [ parens pFOFFormula
  , pConstant
  , pVariable
  ]

prefix :: Text -> (Formula -> Formula) -> Operator Parser Formula
prefix name f = Prefix (foldr1 (.) <$> some (f <$ symbol name))

binary :: Text -> (Formula -> Formula -> Formula) -> Operator Parser Formula
binary name f = InfixR (f <$ symbol name)

operatorTable :: [[Operator Parser Formula]]
operatorTable =
  [ [ prefix "~" Neg ]
  , [ binary "&" (:&) ]
  , [ binary "|" (:|) ]
  , [ binary "=>" (:>) ]
  , [ binary "<=>" (<:>) ]
  ]

-- | Parse FOF formula
pFOFFormula :: Parser Formula
pFOFFormula = makeExprParser pUnitaryFormula operatorTable

-- | Ignore FOF names
pFOFName :: Parser ()
pFOFName = void (takeWhile1P (Just "name") (/= ','))

-- | Parse FOF role
pFOFRole :: Parser Bool
pFOFRole = choice
  [ False <$ symbol "axiom"
  , True <$ symbol "conjecture"
  ]

-- | Parse annotated FOF
pFOF :: Parser (Bool, Formula)
pFOF = do
  symbol "fof" *> symbol "(" *> pFOFName <* symbol ","
  goal <- pFOFRole <* symbol ","
  formula <- pFOFFormula <* symbol ")" <* symbol "."
  return (goal, formula)

-- | Parse TPTP file
pTPTP :: Parser Formula
pTPTP = do
  fs <- sc *> some pFOF <* eof
  let (goals, facts) = partition fst fs
  let combine = foldr1 (:&) . map snd
  let goal = if null goals then Top else combine goals
  return (if null facts then goal else combine facts :> goal)

-- | TPTP Syntax based parser: https://tptp.org/TPTP/SyntaxBNF.html
parseFile :: String -> Text -> Formula
parseFile file input =
  let output = runParserT pTPTP file input
  in case evalState output M.empty of
    Left e  -> error $ errorBundlePretty e
    Right f -> f

-- TODO: merge parseFile and parseFormula
parseFormula :: Text -> (Formula, M.Map String Int)
parseFormula input =
  let output = runParserT pFOFFormula "" input
  in case runState output M.empty of
    (Left e, _)  -> error $ errorBundlePretty e
    (Right f, m) -> (f, m)
