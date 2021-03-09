import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Regex.TDFA
import Text.Show

tokenize :: String -> [String]
tokenize s = getAllTextMatches (s =~ "[@.()]|[^[:space:]@.()]+")

data ParExpression = Tok String
                   | Par [ParExpression]
  deriving (Eq, Show)

prettyShow :: ParExpression -> String
prettyShow (Tok tok) = tok
prettyShow (Par exprs) = "(" ++ (intercalate " " (map prettyShow exprs)) ++ ")"

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
applyChurchNums vars expr
  | null vars = expr
  | otherwise =
    let var = head vars
    in  applyChurchNums (tail vars)
          (if var == "1"
           then Appl (Abst var expr) (Abst "f" (Var "f"))
           else if isDigit var
           then churchNum (read var)
           else expr)

genListBody :: String -> [ParExpression] -> Expression
genListBody endName parExprs
  | null parExprs = Var endName
  | otherwise = Abst "f" (Appl (Appl (Var "f") (parseParExpr [head parExprs]))
                               (genListBody endName (tail parExprs)))

genList :: String -> [ParExpression] -> Expression
genList endName parExprs = Abst endName (genListBody endName parExprs)

data Expression = Var String
                | Appl Expression Expression
                | Abst String Expression
parseParExpr :: [ParExpression] -> Expression
-- Variable
parseParExpr [Tok tok] =
  if isVar(tok)
    then Var tok
    else parseParExpr([Tok tok])
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
