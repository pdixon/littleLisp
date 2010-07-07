module Parser where

import Text.ParserCombinators.Parsec
import Control.Monad.Error
import Expr

parseInteger :: Parser Expr
parseInteger = do sign <- option "" $ string "-"
                  number <- many1 digit
                  return $ LispInt (read (sign++number))

parseSymbol :: Parser Expr
parseSymbol = do f <- intial
                 r <- many (intial <|> digit)
                 return $ LispSymbol (f:r)
    where intial = oneOf "+-*/" <|> letter

parseAtom :: Parser Expr
parseAtom = parseSymbol <|> parseList <|> parseInteger

parseList :: Parser Expr
parseList = do char '('
               skipMany space
               x <- sepBy parseAtom (many1 space)
               char ')'
               return $ LispList x

parseExpr :: Parser Expr
parseExpr = do skipMany space
               x <- parseList
               skipMany space
               eof
               return x

parseToString :: String -> String
parseToString source = case (parse parseExpr "" source) of
                     Right x -> show x
                     Left e -> show e
