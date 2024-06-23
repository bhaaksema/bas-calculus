module Parser (parseTPTP, parseAxiom) where

import           Control.Monad.Combinators.Expr
import           Control.Monad.State
import           Data.Functor                   (void)
import           Data.List                      (partition)
import qualified Data.Map                       as M
import           Data.Void                      (Void)
import           Text.Megaparsec                hiding (State, parse)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

import Data.Formula

type Parser = ParsecT Void String (State (M.Map String Int))

sc :: Parser ()
sc = L.space space1
  (L.skipLineComment "%")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
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

prefix :: String -> (Formula -> Formula) -> Operator Parser Formula
prefix name f = Prefix (foldr1 (.) <$> some (f <$ symbol name))

binary :: String -> (Formula -> Formula -> Formula) -> Operator Parser Formula
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
  fs <- some pFOF <* eof
  let (goals, facts) = partition fst fs
  let combine = foldr1 (:&) . map snd
  let goal = if null goals then Top else combine goals
  return (if null facts then goal else combine facts :> goal)

-- | TPTP Syntax based parser: https://tptp.org/TPTP/SyntaxBNF.html
parseGeneric :: Parser a -> String -> String -> a
parseGeneric parser file input = let
  output = runParserT (sc *> parser) file input
  in case evalState output M.empty of
    Left  err -> error (errorBundlePretty err)
    Right out -> out

parseTPTP :: String -> String -> Formula
parseTPTP = parseGeneric pTPTP

parseAxiom :: String -> [Formula]
parseAxiom = parseGeneric (pFOFFormula `sepBy1` symbol ",") ""
