module Language.Boa.Parser ( parse, parseFile ) where

import           Control.Monad (void)
import           Text.Megaparsec hiding (parse)
import           Data.List.NonEmpty         as NE
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Char
import           Text.Megaparsec.Expr
import           Language.Boa.Types

type Parser = Parsec SourcePos Text
--------------------------------------------------------------------------------
parseFile :: FilePath -> IO (Expr ())
--------------------------------------------------------------------------------
parseFile f = void . parse f <$> readFile f

--------------------------------------------------------------------------------
parse :: FilePath -> Text -> Bare
--------------------------------------------------------------------------------
parse = parseWith expr

parseWith  :: Parser a -> FilePath -> Text -> a
parseWith p f s = case runParser (whole p) f s of
                    Left err -> panic (show err) (posSpan . NE.head . errorPos $ err)
                    Right e  -> e


-- https://mrkkrp.github.io/megaparsec/tutorials/parsing-simple-imperative-language.html

--------------------------------------------------------------------------------
-- | Top-Level Expression Parser
--------------------------------------------------------------------------------
expr :: Parser Bare
expr = makeExprParser expr0 binops

expr0 :: Parser Bare
expr0 =  try primExpr
     <|> try letExpr
     <|> try ifExpr
     <|> try (parens expr)
     <|> try constExpr
     <|> idExpr

--------------------------------------------------------------------------------
-- | Individual Sub-Expression Parsers
--------------------------------------------------------------------------------
binops :: [[Operator Parser Bare]]
binops =
  [ [ InfixL (symbol "*"  *> pure (op Times))
    ]
  , [ InfixL (symbol "+"  *> pure (op Plus))
    , InfixL (symbol "-"  *> pure (op Minus))
    ]
  ]
  where
    op o e1 e2 = Prim2 o e1 e2 (stretch [e1, e2])

idExpr :: Parser Bare
idExpr = uncurry Id <$> identifier

constExpr :: Parser Bare
constExpr
   =  (uncurry Number <$> integer)

primExpr :: Parser Bare
primExpr = withSpan' (Prim1 <$> primOp <*> parens expr)

primOp :: Parser Prim1
primOp
  =  try (rWord "add1"   *> pure Add1)
 <|>     (rWord "sub1"   *> pure Sub1)

letExpr :: Parser Bare
letExpr = withSpan' $ do
  rWord "let"
  bs <- sepBy1 bind comma
  rWord "in"
  e  <- expr
  return (bindsExpr bs e)

bind :: Parser (BareBind, Bare)
bind = (,) <$> binder <* symbol "=" <*> expr

ifExpr :: Parser Bare
ifExpr = withSpan' $ do
  rWord "if"
  b  <- expr
  e1 <- between colon elsecolon expr
  e2 <- expr
  return (If b e1 e2)
  where
   elsecolon = rWord "else" *> colon

--------------------------------------------------------------------------------
-- | Tokenisers and Whitespace
--------------------------------------------------------------------------------

-- | Top-level parsers (should consume all input)
whole :: Parser a -> Parser a
whole p = sc *> p <* eof

-- RJ: rename me "space consumer"
sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

-- | `symbol s` parses just the string s (and trailing whitespace)
symbol :: String -> Parser String
symbol = L.symbol sc

comma :: Parser String
comma = symbol ","

colon :: Parser String
colon = symbol ":"

-- | 'parens' parses something between parenthesis.
parens :: Parser a -> Parser a
parens = betweenS "(" ")"

betweenS :: String -> String -> Parser a -> Parser a
betweenS l r = between (symbol l) (symbol r)

-- | `lexeme p` consume whitespace after running p
lexeme :: Parser a -> Parser (a, SourceSpan)
lexeme p = L.lexeme sc (withSpan p)

-- | 'integer' parses an integer.
integer :: Parser (Integer, SourceSpan)
integer = lexeme L.decimal

-- | `rWord`
rWord   :: String -> Parser SourceSpan
rWord w = snd <$> (withSpan (string w) <* notFollowedBy alphaNumChar <* sc)


-- | list of reserved words
keywords :: [Text]
keywords =
  [ "if"      , "else"
  , "true"    , "false"
  , "let"     , "in"
  , "add1"    , "sub1"  , "isNum"   , "isBool",   "print"
  , "def"     , "lambda"
  ]

withSpan' :: Parser (SourceSpan -> a) -> Parser a
withSpan' p = do
  p1 <- getPosition
  f  <- p
  p2 <- getPosition
  return (f (SS p1 p2))

withSpan :: Parser a -> Parser (a, SourceSpan)
withSpan p = do
  p1 <- getPosition
  x  <- p
  p2 <- getPosition
  return (x, SS p1 p2)

-- | `binder` parses BareBind, used for let-binds and function parameters.
binder :: Parser BareBind
binder = uncurry Bind <$> identifier

-- | `identifier` parses identifiers: lower-case alphabets followed by alphas or digits
identifier :: Parser (String, SourceSpan)
identifier = lexeme (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` keywords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

stretch :: (Monoid a) => [Expr a] -> a
stretch = mconcat . fmap getLabel
