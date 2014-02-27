{-# LANGUAGE OverloadedStrings #-}

module WebParse where

import Control.Applicative ((<$>))
import Data.List (intercalate)
import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import Text.Parsec.Text.Lazy (Parser)

import qualified Datatypes

pGraphLoc :: Parser Datatypes.GraphLoc
pGraphLoc = do
  theory <- P.sepBy1 pSequent $ P.char ';'
  P.char '!'
  initialIndex <- pInt
  steps <- P.many $ do
    P.char ';'
    constraint <- pMAtom
    P.char '!'
    index <- pInt
    return (constraint,index)
  return $ Datatypes.GraphLoc theory initialIndex steps

pSequent :: Parser Datatypes.Sequent
pSequent = do
  premises <- P.sepBy pTAtom $ P.char '*'
  P.char ':'
  consequents <- flip P.sepBy (P.char '/') $ do
    existentials <- P.option [] $ P.between (P.char '?') (P.char '.') $ P.sepBy1 pVariableSymbol $ P.char ','
    atoms <- P.sepBy pTAtom $ P.char '*'
    return (existentials,atoms)
  return $ Datatypes.Sequent premises consequents

pTAtom :: Parser Datatypes.TAtom
pTAtom = pTPredicate <|> pTEquality

pTPredicate :: Parser Datatypes.TAtom
pTPredicate = do
  symbol <- pPredicateSymbol
  args <- P.between (P.char '(') (P.char ')') $ P.sepBy1 pTTerm $ P.char ','
  return $ Datatypes.TPredicate symbol args

pTEquality :: Parser Datatypes.TAtom
pTEquality = do
  term1 <- pTTerm
  P.char '='
  term2 <- pTTerm
  return $ Datatypes.TEquality term1 term2

pTTerm :: Parser Datatypes.TTerm
pTTerm = pTVariable <|> pTFunction

pTVariable :: Parser Datatypes.TTerm
pTVariable = do
  P.char '.'
  symbol <- pVariableSymbol
  return $ Datatypes.TVariable symbol

pTFunction :: Parser Datatypes.TTerm
pTFunction = do
  P.char '@'
  symbol <- pFunctionSymbol
  args <- P.between (P.char '(') (P.char ')') $ P.sepBy1 pTTerm $ P.char ','
  return $ Datatypes.TFunction symbol args

pMAtom :: Parser Datatypes.MAtom
pMAtom = pMPredicate <|> pMEquality

pMPredicate :: Parser Datatypes.MAtom
pMPredicate = do
  symbol <- pPredicateSymbol
  args <- P.between (P.char '(') (P.char ')') $ P.sepBy pMTerm $ P.char ','
  return $ Datatypes.MPredicate symbol args
  
pMEquality :: Parser Datatypes.MAtom
pMEquality = do
  term1 <- pMTerm
  P.char '='
  term2 <- pMTerm
  return $ Datatypes.MEquality term1 term2

pMTerm :: Parser Datatypes.MTerm
pMTerm = pMVariable <|> pMFunction <|> pModelElement

pMVariable :: Parser Datatypes.MTerm
pMVariable = do
  P.char '.'
  symbol <- pVariableSymbol
  return $ Datatypes.MVariable symbol

pMFunction :: Parser Datatypes.MTerm
pMFunction = do
  P.char '@'
  symbol <- pFunctionSymbol
  args <- P.between (P.char '(') (P.char ')') $ P.sepBy pMTerm $ P.char ','
  return $ Datatypes.MFunction symbol args

pModelElement :: Parser Datatypes.MTerm
pModelElement = Datatypes.ModelElement <$> pModelElementSymbol

pVariableSymbol :: Parser Datatypes.VariableSymbol
pVariableSymbol = Datatypes.VariableSymbol <$> P.many1 P.alphaNum

pPredicateSymbol :: Parser Datatypes.PredicateSymbol
pPredicateSymbol = Datatypes.PredicateSymbol <$> P.many1 P.alphaNum

pFunctionSymbol :: Parser Datatypes.FunctionSymbol
pFunctionSymbol = Datatypes.FunctionSymbol <$> P.many1 P.alphaNum

pModelElementSymbol :: Parser Datatypes.ModelElementSymbol
pModelElementSymbol = do
  P.char '$'
  symnum <- pInt
  return $ Datatypes.ModelElementSymbol symnum

pInt :: Parser Int
pInt = read <$> P.many1 P.digit

encodeGraphLoc :: Datatypes.GraphLoc -> String
encodeGraphLoc (Datatypes.GraphLoc theory initialIndex steps) =
  intercalate ";" (map encodeSequent theory) ++ "!" ++ show initialIndex ++
  concat [";" ++ encodeMAtom constraint ++ "!" ++ show index |
          (constraint,index) <- steps]

encodeSequent :: Datatypes.Sequent -> String
encodeSequent (Datatypes.Sequent premises consequents) =
  intercalate "*" (map encodeTAtom premises) ++ ":" ++
  intercalate "/"
  [(if null existentials then "" else
      "?" ++ intercalate "," (map encodeVariableSymbol existentials) ++ ".") ++
   intercalate "*" (map encodeTAtom atoms) |
   (existentials,atoms) <- consequents]

encodeTAtom :: Datatypes.TAtom -> String
encodeTAtom (Datatypes.TPredicate symbol args) =
  encodePredicateSymbol symbol ++ "(" ++
  intercalate "," (map encodeTTerm args) ++ ")"
encodeTAtom (Datatypes.TEquality term1 term2) =
  encodeTTerm term1 ++ "=" ++ encodeTTerm term2

encodeTTerm :: Datatypes.TTerm -> String
encodeTTerm (Datatypes.TVariable symbol) = "." ++ encodeVariableSymbol symbol
encodeTTerm (Datatypes.TFunction symbol args) =
  "@" ++ encodeFunctionSymbol symbol ++ "(" ++
  intercalate "," (map encodeTTerm args) ++ ")"

encodeMAtom :: Datatypes.MAtom -> String
encodeMAtom (Datatypes.MPredicate symbol args) =
  encodePredicateSymbol symbol ++ "(" ++
  intercalate "," (map encodeMTerm args) ++ ")"
encodeMAtom (Datatypes.MEquality term1 term2) =
  encodeMTerm term1 ++ "=" ++ encodeMTerm term2

encodeMTerm :: Datatypes.MTerm -> String
encodeMTerm (Datatypes.MVariable symbol) = "." ++ encodeVariableSymbol symbol
encodeMTerm (Datatypes.MFunction symbol args) =
  "@" ++ encodeFunctionSymbol symbol ++ "(" ++
  intercalate "," (map encodeMTerm args) ++ ")"
encodeMTerm (Datatypes.ModelElement symbol) = encodeModelElementSymbol symbol

encodeVariableSymbol :: Datatypes.VariableSymbol -> String
encodeVariableSymbol (Datatypes.VariableSymbol symname) = symname

encodePredicateSymbol :: Datatypes.PredicateSymbol -> String
encodePredicateSymbol (Datatypes.PredicateSymbol symname) = symname

encodeFunctionSymbol :: Datatypes.FunctionSymbol -> String
encodeFunctionSymbol (Datatypes.FunctionSymbol symname) = symname

encodeModelElementSymbol :: Datatypes.ModelElementSymbol -> String
encodeModelElementSymbol (Datatypes.ModelElementSymbol symnum) =
  "$" ++ show symnum