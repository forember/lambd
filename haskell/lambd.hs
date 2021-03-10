import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Regex.TDFA
import Text.Show

-- Syntax ---------------------------------------------------------------------

tokenize :: String -> [String]
tokenize s = getAllTextMatches (s =~ "[@.()]|[^[:space:]@.()]+")

data ParExpression = Tok String
                   | Par [ParExpression]
  deriving (Eq, Show)

prettyParExpr :: ParExpression -> String
prettyParExpr (Tok tok) = tok
prettyParExpr (Par exprs) =
  "(" ++ (intercalate " " (map prettyParExpr exprs)) ++ ")"

isLess :: Maybe Int -> Maybe Int -> Bool
isLess Nothing Nothing = False
isLess Nothing (Just b) = False
isLess (Just a) Nothing = True
isLess (Just a) (Just b) = (a < b)

parseParTree :: [String] -> ([ParExpression], Int)
parseParTree toks
  | (isLess (elemIndex "(" toks) (elemIndex ")" toks)) =
    let (beforeOpen, fromOpen) = span (/= "(") toks
        (parExpr, parLength) = parseParTree (tail fromOpen)
        (restExpr, restLength) = parseParTree (drop (parLength + 2) fromOpen)
    in  ((map Tok beforeOpen) ++ ([Par parExpr]) ++ restExpr,
         (length beforeOpen) + parLength + 2 + restLength)
  | otherwise =
    let beforeClose = takeWhile (/= ")") toks
    in  (map Tok beforeClose, length beforeClose)

data Expression = Var String
                | Appl Expression Expression
                | Abst String Expression
  deriving (Eq, Show)

isVar :: String -> Bool
isVar tok = (tok =~ "^[^[:space:]@.()]+$")

splitTree :: String -> [ParExpression] -> ([ParExpression],[ParExpression])
splitTree tok tree =
  let (extra, body) = span (/= (Tok tok)) tree
  in  (extra, tail body)

freeVars :: Expression -> Set String
freeVars (Var name) = Set.singleton name
freeVars (Abst name expr) = Set.delete name (freeVars expr)
freeVars (Appl exprA exprB) = Set.union (freeVars exprA) (freeVars exprB)

isDigit :: String -> Bool
isDigit tok = (tok =~ "^[[:digit:]]+$")

churchNumBody :: Int -> Expression
churchNumBody num
  | num > 0 = Appl (Var "f") (churchNumBody (num - 1))
  | otherwise = Var "x"

churchNum :: Int -> Expression
churchNum num = Abst "f" (Abst "x" (churchNumBody num))

applyChurchNums :: [String] -> Expression -> Expression
applyChurchNums vars body
  | null vars = body
  | otherwise =
    let var = head vars
    in  applyChurchNums (tail vars)
          (if var == "1"
           then Appl (Abst var body) (Abst "f" (Var "f"))
           else if isDigit var
           then Appl (Abst var body) (churchNum (read var))
           else body)

genListBody :: String -> [ParExpression] -> Expression
genListBody endName parExprs
  | null parExprs = Var endName
  | otherwise = Abst "f" (Appl (Appl (Var "f") (parseParExpr [head parExprs]))
                               (genListBody endName (tail parExprs)))

genList :: String -> [ParExpression] -> Expression
genList endName parExprs = Abst endName (genListBody endName parExprs)

genAbstraction :: [ParExpression] -> Expression -> Expression
genAbstraction vars body
  | null vars = body
  | otherwise =
    let Tok name = head vars
    in  Abst name (genAbstraction (tail vars) body)

genApplication :: [Expression] -> Expression
genApplication [expr] = expr
genApplication [exprA, exprB] = Appl exprA exprB
genApplication exprs = Appl (genApplication (init exprs)) (last exprs)

parseParExpr :: [ParExpression] -> Expression
parseParExpr [Par tree] = parseParExpr tree
-- Variable
parseParExpr [Tok tok] =
  if isVar tok
    then Var tok
    else parseParExpr [Tok tok]
-- Extensions
parseParExpr ((Tok "@"):(Tok "@"):(Tok "b"):tree) =
  let (extra, body) = splitTree "." tree
      Tok extraname = if null extra then Tok "" else head extra
      name = "@b" ++ if null extra then "" else " " ++ extraname
  in  Appl (Abst name (Var name)) (parseParExpr body)
parseParExpr ((Tok "@"):(Tok "@"):(Tok "c"):tree) =
  let (extra, body) = splitTree "." tree
  in  parseParExpr (tail body)
parseParExpr ((Tok "@"):(Tok "@"):(Tok "churchnums"):tree) =
  let (extra, body) = splitTree "." tree
      parsed = parseParExpr body
  in  applyChurchNums (Set.toList (freeVars parsed)) parsed
parseParExpr ((Tok "@"):(Tok "@"):(Tok "def"):tree) =
  let (extra, body) = splitTree "." tree
      Tok name = head extra
  in  Appl (Abst name (parseParExpr (tail body))) (parseParExpr [head body])
parseParExpr ((Tok "@"):(Tok "@"):(Tok "include"):tree) =
  let (extra, body) = splitTree "." tree
      Tok name = head extra
  in  parseParExpr body -- TODO
parseParExpr ((Tok "@"):(Tok "@"):(Tok "list"):tree) =
  let (extra, body) = splitTree "." tree
      Tok name = head extra
  in  genList name body
parseParExpr ((Tok "@"):(Tok "@"):(Tok "n"):tree) =
  let (extra, body) = splitTree "." tree
  in  parseParExpr body
-- Abstraction
parseParExpr ((Tok "@"):tree) =
  let (extra, body) = splitTree "." tree
  in  genAbstraction extra (parseParExpr body)
-- Application
parseParExpr tree =
  if elem (Tok "@") tree
  then let (beforeLambda, afterLambda) = splitTree "@" tree
           (extra, body) = splitTree "." afterLambda
       in  genApplication ((map (\p -> parseParExpr [p]) beforeLambda
                        ++ [genAbstraction extra (parseParExpr body)]))
  else genApplication (map (\p -> parseParExpr [p]) tree)

unparse :: Expression -> String
unparse (Var name) = name
unparse (Abst name expr) = "@" ++ name ++ "." ++ (unparse expr)
unparse (Appl (Abst nameA exprA) (Var nameB)) =
  "(" ++ (unparse (Abst nameA exprA)) ++ ")" ++ nameB
unparse (Appl (Abst nameA exprA) exprB) =
  "(" ++ (unparse (Abst nameA exprA)) ++ ")(" ++ (unparse exprB) ++ ")"
unparse (Appl exprA (Var nameB)) = (unparse exprA) ++ " " ++ nameB
unparse (Appl exprA exprB) = (unparse exprA) ++ "(" ++ (unparse exprB) ++ ")"

indentPrettyExpr :: Expression -> String -> String
indentPrettyExpr (Var name) indent = indent ++ "(Var " ++ (show name) ++ ")"
indentPrettyExpr (Abst name expr) indent =
  indent ++ "(Abst " ++ (show name) ++ "\n"
  ++ (indentPrettyExpr expr (indent ++ " ")) ++ "\n" ++ indent ++ ")"
indentPrettyExpr (Appl exprA exprB) indent =
  indent ++ "(Appl \n" ++  (indentPrettyExpr exprA (indent ++ " ")) ++ "\n"
  ++ (indentPrettyExpr exprB (indent ++ " ")) ++ "\n" ++ indent ++ ")"

isChurchNumBody :: String -> String -> Int -> Expression -> Maybe Int
isChurchNumBody f x i (Appl (Var a) expr) =
  if f == a then isChurchNumBody f x (i + 1) expr else Nothing
isChurchNumBody f x i (Var a) = if x == a then Just i else Nothing
isChurchNumBody f x i expr = Nothing

restoreNums :: Expression -> Expression
restoreNums (Abst a (Var b)) = if a == b then (Var "1") else (Abst a (Var b))
restoreNums (Abst a (Abst b body)) =
  maybe (Abst a (restoreNums (Abst b body))) (\n -> Var (show n))
      (isChurchNumBody a b 0 body)
restoreNums (Var a) = Var a
restoreNums (Abst a body) = Abst a (restoreNums body)
restoreNums (Appl exprA exprB) = Appl (restoreNums exprA) (restoreNums exprB)

prettyExpr :: Expression -> String
prettyExpr expr = indentPrettyExpr expr ""

-- Semantics ------------------------------------------------------------------
