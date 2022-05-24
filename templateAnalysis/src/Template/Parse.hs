module Template.Parse where

import Template

import Text.ParserCombinators.ReadP
import Data.Char

parseTemplates :: String -> String -> [LabeledTemplate]
parseTemplates src s = case
  readP_to_S (between (char '[') (char ']') (sepBy (parseLabTemplate src) (char ',')))
  (filter (\c -> (not $ isSpace c)) s) of
  [(x, [])] -> x
  --ps -> error ("parse': got result " ++ show ps ++ " while parsing " ++ s) -- replace with empty list?
  _ -> []
parseLabTemplate :: String -> ReadP LabeledTemplate
parseLabTemplate src = do
  _ <- char '('
  label <- between (char '"') (char '"') (munch (/= '"'))
  _ <- char ','
  lemmastring <- between (char '"') (char '"') (munch (/= '"'))
  _ <- char ','
  t <- parseTemplate
  _ <- char ')'
  let tinfo = (label,lemmastring,src)
  return (tinfo,t)

parseT :: String -> PropTemplate
parseT s = case readP_to_S parseTemplate (filter (not . isSpace) s) of
  [(x, [])] -> x
  ps -> error ("parse': got result " ++ show ps ++ " while parsing " ++ s)

parseEq :: ReadP PropTemplate
parseEq = do
  _ <- string "template_equation"
  _ <- char '('
  lhs <- parseTerm
  _ <- char ','
  rhs <- parseTerm
  _ <- char ')'
  return $ Equation Equ (lhs,rhs)

parsePred :: ReadP PropTemplate
parsePred = do
  _ <- string "template_predicate"
  _ <- char '('
  stmt <- parseTerm
  _ <- char ')'
  return $ Predicate stmt

parseImp :: ReadP PropTemplate
parseImp = do
  _ <- string "template_implication"
  _ <- char '('
  lhs <- between (char '[') (char ']') (sepBy (parseTemplate) (char ','))
  _ <- char ','
  rhs <- parseTemplate
  _ <- char ')'
  return $ Implication lhs rhs

parseBImp :: ReadP PropTemplate
parseBImp = do
  _ <- string "template_bimplication"
  _ <- char '('
  lhs <- parseTemplate
  _ <- char ','
  rhs <- parseTemplate
  _ <- char ')'
  return $ BImplication lhs rhs

parseNeg :: ReadP PropTemplate
parseNeg = do
  _ <- string "template_negation"
  t <- parseTemplate
  return $ Negation t

parseInEq :: ReadP PropTemplate
parseInEq = do
  _ <- string "template_inequation"
  _ <- char '('
  s <- parseSign
  _ <- char ','
  t1 <- parseTerm
  _ <- char ','
  t2 <- parseTerm
  _ <- char ')'
  return $ Equation s (t1, t2)

parseSign :: ReadP Sign
parseSign = parseEqu <++ parseGE <++ parseGrT <++ parseLE <++ parseLeT
  where
    parseEqu = do
      _ <- string "equals"
      return Equ
    parseGE = do
      _ <- string "greater_equals"
      return GE
    parseGrT = do
      _ <- string "greater_than"
      return GrT
    parseLE = do
      _ <- string "less_equals"
      return LE
    parseLeT = do
      _ <- string "less_than"
      return LeT

parseUnknown :: ReadP PropTemplate
parseUnknown = do
  _ <- string "template_dunno"
  return Unknown

parseTemplate :: ReadP PropTemplate
parseTemplate = parseUnknown <++ parseEq <++ parseImp <++ parseBImp <++ parsePred <++ parseNeg <++ parseInEq

parseTerm :: ReadP TermTemplate
parseTerm = parseEmpty <++ parseApp <++ parseHole <++ parseVar

parseVar :: ReadP TermTemplate
parseVar = do
  _ <- string "template_var"
  n <- munch1 isNumber
  let k = sum [(digitToInt i)*(10^(length n - j)) | (i,j) <- zip n [1..]]
    in return $ TVar k


parseHole :: ReadP TermTemplate
parseHole = do
  _ <- string "template_hole"
  n <- munch1 isNumber
  let k = sum [(digitToInt i)*(10^(length n - j)) | (i,j) <- zip n [1..]]
    in return $ THole k

parseApp :: ReadP TermTemplate
parseApp = do
  _ <- string "template_app"
  _ <- char '('
  f <- parseTerm
  _ <- char ','
  arg <- parseTerm
  _ <- char ')'
  return $ TApp f arg

parseEmpty :: ReadP TermTemplate
parseEmpty = do
  _ <- string "t_empty"
  return TEmpty
