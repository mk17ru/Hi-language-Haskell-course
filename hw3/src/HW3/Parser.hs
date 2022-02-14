{-# LANGUAGE OverloadedStrings #-}

module HW3.Parser
  ( parse,
  )
where

import Control.Monad.Combinators.Expr
import Data.ByteString as Ch (pack)
import Data.Char (isAlpha, isAlphaNum)
import qualified Data.List as L (intercalate)
import qualified Data.Map as M ()
import Data.Ratio (Rational)
import Data.Scientific (Scientific)
import Data.Sequence ()
import qualified Data.Text as T (Text, pack)
import Data.Void (Void)
import GHC.Real hiding (Rational)
import GHC.Word (Word8)
import HW3.Base
import HW3.Common
import Numeric (readHex)
import Text.Megaparsec
  ( Parsec,
    between,
    choice,
    count,
    empty,
    eof,
    many,
    manyTill,
    notFollowedBy,
    runParser,
    satisfy,
    sepBy,
    sepBy1,
    sepEndBy,
    try,
    (<|>),
  )
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer ()
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (ParseErrorBundle)

-- | Parser which will use for parsing, create by Parsec from megaparsec
type Parser = Parsec Void String

-- | All HiFunction
allFuncs :: [HiFun]
allFuncs = [(minBound :: HiFun) ..]

-- | Action which already ready for parsing
actionForParsing :: [HiAction]
actionForParsing = [HiActionNow, HiActionCwd]

-- | Parse Bool
parseBoolValue :: Parser HiValue
parseBoolValue =
  choice
    [ HiValueBool True <$ symbol "true",
      HiValueBool False <$ symbol "false"
    ]

-- | parse number
parseNum :: Parser Scientific
parseNum = L.signed space L.scientific <* space

-- | parse action which already ready for parsing
parseAction :: HiAction -> Parser HiValue
parseAction action = HiValueAction action <$ symbol (actionName action)

-- | parse actions
parseActions :: Parser HiValue
parseActions = choice (map parseAction actionForParsing)

-- | parse Null
parseNullValue :: Parser HiValue
parseNullValue = HiValueNull <$ symbol "null"

-- | parse string in quotes
stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

-- | parse String HiValue
parseStringValue :: Parser HiValue
parseStringValue = lexeme $ HiValueString . T.pack <$> stringLiteral

-- | parse Number HiValue
parseNumValue :: Parser HiValue
parseNumValue = HiValueNumber . toRational <$> parseNum

-- | parse  function
parserFunc :: HiFun -> Parser HiFun
parserFunc func = func <$ symbol (funName func)

-- | try parse Each functions
parseFuncNameValue :: Parser HiValue
parseFuncNameValue = HiValueFunction <$> choice (map parserFunc allFuncs)

-- | parse 2 Bytes
parseBytes :: Parser Word8
parseBytes = fst . head . readHex <$> count 2 hexDigitChar

-- | parse ByteArray HiValue
parseByteArray :: Parser HiValue
parseByteArray = HiValueBytes . Ch.pack <$> try (lexeme $ bracArray (parseBytes `sepEndBy` space1))

-- | parse Hivalues
parseValue :: Parser HiValue
parseValue =
  parseNumValue
    <|> parseNullValue
    <|> parseStringValue
    <|> parseBoolValue
    <|> parseFuncNameValue
    <|> parseActions
    <|> parseByteArray

-- | parse one symbol and skip whitespaces
symbol :: String -> Parser String
symbol = L.symbol space

-- | parse any around symbols
brac :: String -> String -> Parser a -> Parser a
brac st end = between (symbol st) (symbol end)

-- | parse bytes brackets
bracArray :: Parser [Word8] -> Parser [Word8]
bracArray = brac "[" "]" <$> brac "#" "#"

-- | parse list brackets
bracSq :: Parser [HiExpr] -> Parser [HiExpr]
bracSq = brac "[" "]"

-- parse dict brackets
bracF :: Parser [(HiExpr, HiExpr)] -> Parser [(HiExpr, HiExpr)]
bracF = brac "{" "}"

-- | parse simple brackets
bracC :: Parser [HiExpr] -> Parser [HiExpr]
bracC = brac "(" ")"

-- | parce lexeme and skip whitespaces after
lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

-- | given pattern for parsing after "."
getCharacters :: Parser String
getCharacters = L.intercalate "-" <$> (((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-')

-- | choice parser for value
parseVal :: Parser HiExpr
parseVal =
  lexeme $
    choice
      [ HiExprValue <$> parseValue,
        parseListVal,
        parseMap,
        symbol "(" *> pExpr <* symbol ")"
      ]

-- | parse map entry { ... : ...}
parseEntry :: Parser (HiExpr, HiExpr)
parseEntry =
  do
    key <- pExpr
    _ <- symbol ":"
    val <- pExpr
    return (key, val)

-- | parse HiExprDict
parseMap :: Parser HiExpr
parseMap = lexeme $ HiExprDict <$> bracF (parseEntry `sepBy` symbol ",")

-- | parser for postfix constructions
data ParsePostfix
  = Func [HiExpr]
  | Access T.Text
  | ExprRun

{-|
  parse postfix
  there is three options:
  1) (expr)
  2) some-text.some-text
  3) some-text!
-}
parsePost :: Parser ParsePostfix
parsePost =
  choice
    [ Func <$> bracC (pExpr `sepBy` symbol ","),
      Access . T.pack <$> lexeme (string "." *> getCharacters),
      ExprRun <$ symbol "!"
    ]

-- | function to convert ParsePostfix to HiExpr
convertPostfix :: HiExpr -> ParsePostfix -> HiExpr
convertPostfix expr postfix =
  case postfix of
    ExprRun -> HiExprRun expr
    Func args -> HiExprApply expr args
    Access text -> HiExprApply expr ((: []) $ HiExprValue $ HiValueString text)

-- | parse arguments for funtion
parseArgs :: HiExpr -> Parser HiExpr
parseArgs func = do
  args <- lexeme (many parsePost)
  return (foldl convertPostfix func args)

-- | parse Expression With Postfix
parseExprWithPostfix :: Parser HiExpr
parseExprWithPostfix =
  do
    func <- parseVal
    parseArgs func

{-|
  parse list
  for example: [1, 2, ...]
-}
parseListVal :: Parser HiExpr
parseListVal = do
  HiExprApply (HiExprValue $ HiValueFunction HiFunList) <$> bracSq (pExpr `sepBy` symbol ",")

-- | pTerm in megaparsec
pTerm :: Parser HiExpr
pTerm =
  choice
    [ try parseExprWithPostfix,
      parseVal
    ]

-- | table where write a declaration for each operator
operatorTable :: [[Operator Parser HiExpr]]
operatorTable =
  [ [ InfixL $ binary "*" $ createBinary HiFunMul,
      InfixL $ binary "/" $ createBinary HiFunDiv
    ],
    [ InfixL $ binary "+" $ createBinary HiFunAdd,
      InfixL $ binary "-" $ createBinary HiFunSub
    ],
    [ InfixN $ binary "<=" $ createBinary HiFunNotGreaterThan,
      InfixN $ binary ">=" $ createBinary HiFunNotLessThan,
      InfixN $ binary "<" $ createBinary HiFunLessThan,
      InfixN $ binary ">" $ createBinary HiFunGreaterThan,
      InfixN $ binary "==" $ createBinary HiFunEquals,
      InfixN $ binary "/=" $ createBinary HiFunNotEquals
    ],
    [InfixR $ binary "&&" $ createBinary HiFunAnd],
    [InfixR $ binary "||" $ createBinary HiFunOr]
  ]

-- | create binary function
createBinary :: HiFun -> (HiExpr -> HiExpr -> HiExpr)
createBinary func x y = HiExprApply (HiExprValue $ HiValueFunction func) [x, y]

-- | create binary operator
binary ::
  String ->
  (HiExpr -> HiExpr -> HiExpr) ->
  Parser (HiExpr -> HiExpr -> HiExpr)
binary "/" f = f <$ (lexeme . try) (string "/" <* notFollowedBy "=")
binary name f = f <$ symbol name

-- | create expression
pExpr :: Parser HiExpr
pExpr = makeExprParser pTerm operatorTable

-- | outer function for parse
parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (space *> pExpr <* eof) ""
