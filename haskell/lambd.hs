{-|
Module      : $Header$
Description : Lambda calculus parser-reducer with some extensions.
Copyright   : (c) Emberlynn McKinney, 2021
License     : MIT
Maintainer  : em@embermckinney.com
Stability   : experimental
Portability : portable
-}

module Main where

import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import System.Environment
import System.IO
import Text.Regex.TDFA
import Text.Show

-- Syntax --------------------------------------------------------------------- -------------------

tokenize :: String -> [String]
-- ^The 'tokenize' function splits a source string into tokens.
tokenize s = getAllTextMatches (s =~ "[@.()]|[^[:space:]@.()]+")

data ParExpression -- ^A 'ParExpression' is a token list with parentheticals identified.
  = Tok String          -- ^A 'Tok' is a single token.
  | Par [ParExpression] -- ^A 'Par' is a parenthetical.
  deriving (Eq, Show)

prettyParExpr :: ParExpression -> String
-- ^The 'prettyParExpr' function represents a 'ParExpression' in a human-readable way.
prettyParExpr (Tok tok) = tok
prettyParExpr (Par exprs) =
  "(" ++ (intercalate " " (map prettyParExpr exprs)) ++ ")"

isLess :: Maybe Int -> Maybe Int -> Bool
-- ^The 'isLess' function does /less than/ comparison, treating 'Nothing' as infinite.
isLess Nothing Nothing = False
isLess Nothing (Just b) = False
isLess (Just a) Nothing = True
isLess (Just a) (Just b) = (a < b)

parseParTree :: [String] -> ([ParExpression], Int)
-- ^The 'parseParTree' function identifies parentheticals in a token list.
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

data Expression -- ^An 'Expression' is the abstract syntax tree of a lambda calculus expression.
  = Var String                  -- ^A 'Var' is a variable name, e.g. @x@.
  | Appl Expression Expression  -- ^An 'Appl' is a function application, e.g. @x y@.
  | Abst String Expression      -- ^An 'Abst' is a function abstraction, e.g. @\@x.x@.
  deriving (Eq, Show)

isVar :: String -> Bool
-- ^The 'isVar' function determines if a token is a variable name.
isVar tok = (tok =~ "^[^[:space:]@.()]+$")

splitTree :: String -> [ParExpression] -> ([ParExpression], [ParExpression])
-- ^The 'splitTree' function splits a list of parenthetical expressions at a specific token.
splitTree tok tree =
  let (extra, body) = span (/= (Tok tok)) tree
  in  (extra, tail body)

freeVars :: Expression -> Set String
-- ^The 'freeVars' function identifies the free variables in an 'Expression'.
freeVars (Var name) = Set.singleton name
freeVars (Abst name expr) = Set.delete name (freeVars expr)
freeVars (Appl exprA exprB) = Set.union (freeVars exprA) (freeVars exprB)

isDigit :: String -> Bool
-- ^The 'isDigit' function determines if a string is composed entirely of digit characters.
isDigit tok = (tok =~ "^[[:digit:]]+$")

churchNumBody :: Int -> Expression
-- ^The 'churchNumBody' function generates the body of the specified Church numeral.
churchNumBody num
  | num > 0 = Appl (Var "f") (churchNumBody (num - 1))
  | otherwise = Var "x"

churchNum :: Int -> Expression
-- ^The 'churchNum' function generates the specified Church numeral.
churchNum num = Abst "f" (Abst "x" (churchNumBody num))

applyChurchNums :: [String] -> Expression -> Expression
-- ^The 'applyChurchNums' function applies the corresponding Church numeral to free numeric
--  variables in an 'Expression'.
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
-- ^The 'genListBody' function generates the body of a list for the @list@ extension.
genListBody endName parExprs
  | null parExprs = Var endName
  | otherwise = Abst "f" (Appl (Appl (Var "f")
      (parseParExpr (Inc {includes=[], included=Set.empty}) [head parExprs]))
    (genListBody endName (tail parExprs)))

genList :: String -> [ParExpression] -> Expression
-- ^The 'genList' function generates a list for the @list@ extension.
genList endName parExprs = Abst endName (genListBody endName parExprs)

genAbstraction :: [ParExpression] -> Expression -> Expression
-- ^The 'genAbstraction' function generates proper abstraction expressions from a potentially
-- multiple-binding abstraction specification.
genAbstraction vars body
  | null vars = body
  | otherwise =
    let Tok name = head vars
    in  Abst name (genAbstraction (tail vars) body)

genApplication :: [Expression] -> Expression
-- ^The 'genApplication' function generates proper left-associated application expressions.
genApplication [expr] = expr
genApplication [exprA, exprB] = Appl exprA exprB
genApplication exprs = Appl (genApplication (init exprs)) (last exprs)

data Inc = Inc {includes::[(String, [ParExpression])], included::Set String}

parseExtension :: Inc -> String -> ([ParExpression], [ParExpression]) -> Expression
-- ^The 'parseExtension' function handles extension to the lambda calculus syntax.
parseExtension inc "b" (extra, body) =
  let Tok extraname = if null extra then Tok "" else head extra
      name = "@b" ++ if null extra then "" else " " ++ extraname
  in  Appl (Abst name (Var name)) (parseParExpr inc body)
parseExtension inc "c" (extra, body) = parseParExpr inc (tail body)
parseExtension inc "churchnums" (extra, body) =
  let parsed = parseParExpr inc body
  in  applyChurchNums (Set.toList (freeVars parsed)) parsed
parseExtension inc "def" (extra, body) =
  let Tok name = head extra
      exprA = parseParExpr inc (tail body)
      exprB = parseParExpr inc [head body]
  in  Appl (Abst name exprA) exprB
parseExtension inc "include" (extra, body) =
  let Tok name = head extra
      newIncluded = Set.insert name (included inc)
      newInc = Inc {includes=includes inc, included=newIncluded}
      fullBody = maybe body (\i -> body ++ i) (lookup name (includes inc))
  in  parseParExpr newInc fullBody
parseExtension inc "list" (extra, body) =
  let Tok name = head extra
  in  genList name body
parseExtension inc _ (extra, body) = parseParExpr inc body

parseParExpr :: Inc -> [ParExpression] -> Expression
-- ^The 'parseParExpr' function generates an abstract syntax tree from a parenthetical token list.
parseParExpr inc [Par tree] = parseParExpr inc tree
-- Variable
parseParExpr inc [Tok tok]
  | isVar tok = Var tok
  | otherwise = parseParExpr inc [Tok tok]
-- Extension
parseParExpr inc ((Tok "@"):(Tok "@"):(Tok extension):tree) =
  parseExtension inc extension (splitTree "." tree)
-- Abstraction
parseParExpr inc ((Tok "@"):tree) =
  let (extra, body) = splitTree "." tree
  in  genAbstraction extra (parseParExpr inc body)
-- Application
parseParExpr inc tree
  | elem (Tok "@") tree =
    let (beforeLambda, afterLambda) = splitTree "@" tree
        (extra, body) = splitTree "." afterLambda
    in  genApplication (map (\p -> parseParExpr inc [p]) beforeLambda
                     ++ [genAbstraction extra (parseParExpr inc body)])
  | otherwise =
    genApplication (map (\p -> parseParExpr inc [p]) tree)

preLoadIncludes :: Set String -> [ParExpression] -> IO [(String, [ParExpression])]
-- ^The 'preLoadIncludes' function preloads all the included files.
preLoadIncludes inc ((Tok "@"):(Tok "@"):(Tok "include"):(Tok name):tree) = 
  let newInc = (Set.insert name inc)
  in  do
      h <- openFile (name ++ ".lambd") ReadMode
      contents <- hGetContents h
      parExprs <- return (fst (parseParTree (tokenize contents)))
      subIncludes <- preLoadIncludes newInc parExprs
      allInc <- return (Set.union newInc (Set.fromList (map fst subIncludes)))
      postIncludes <- preLoadIncludes allInc tree
      return ([(name, parExprs)] ++ subIncludes ++ postIncludes)
preLoadIncludes inc [] = return []
preLoadIncludes inc (_:tree) = preLoadIncludes inc tree

unparse :: Expression -> String
-- ^The 'unparse' function converts an 'Expression' back to a source string.
unparse (Var name) = name
unparse (Abst name expr) = "@" ++ name ++ "." ++ (unparse expr)
unparse (Appl (Abst nameA exprA) (Var nameB)) =
  "(" ++ (unparse (Abst nameA exprA)) ++ ")" ++ nameB
unparse (Appl (Abst nameA exprA) exprB) =
  "(" ++ (unparse (Abst nameA exprA)) ++ ")(" ++ (unparse exprB) ++ ")"
unparse (Appl exprA (Var nameB)) = (unparse exprA) ++ " " ++ nameB
unparse (Appl exprA exprB) = (unparse exprA) ++ "(" ++ (unparse exprB) ++ ")"

isChurchNumBody :: String -> String -> Int -> Expression -> Maybe Int
-- ^The 'isChurchNumBody' function identifies the body of a church numeral.
isChurchNumBody f x i (Appl (Var a) expr)
  | f == a = isChurchNumBody f x (i + 1) expr
  | otherwise = Nothing
isChurchNumBody f x i (Var a)
  | x == a = Just i
  | otherwise = Nothing
isChurchNumBody f x i expr = Nothing

restoreNums :: Expression -> Expression
-- ^The 'restoreNums' function identifies church numerals and replaces them with variable names.
restoreNums (Abst a (Var b))
  | a == b = Var "1"
  | otherwise = Abst a (Var b)
restoreNums (Abst a (Abst b body)) =
  maybe (Abst a (restoreNums (Abst b body))) (\n -> Var (show n))
      (isChurchNumBody a b 0 body)
restoreNums (Var a) = Var a
restoreNums (Abst a body) = Abst a (restoreNums body)
restoreNums (Appl exprA exprB) = Appl (restoreNums exprA) (restoreNums exprB)

indentPrettyExpr :: Expression -> String -> String
-- ^The 'indentPrettyExpr' function represents an 'Expression' with indentation.
indentPrettyExpr (Var name) indent = indent ++ "(Var " ++ (show name) ++ ")"
indentPrettyExpr (Abst name expr) indent =
  indent ++ "(Abst " ++ (show name) ++ "\n"
  ++ (indentPrettyExpr expr (indent ++ " ")) ++ "\n" ++ indent ++ ")"
indentPrettyExpr (Appl exprA exprB) indent =
  indent ++ "(Appl \n" ++  (indentPrettyExpr exprA (indent ++ " ")) ++ "\n"
  ++ (indentPrettyExpr exprB (indent ++ " ")) ++ "\n" ++ indent ++ ")"

prettyExpr :: Expression -> String
-- ^The 'prettyExpr' function is shorthand for 'indentPrettyExpr' /expr/ @""@.
prettyExpr expr = indentPrettyExpr expr ""

parse :: String -> Expression
-- ^The 'parse' function parses a source string into an abstract syntax tree.
parse s =
  let (parExpr, _) = parseParTree (tokenize s)
  in  parseParExpr (Inc {includes=[], included=Set.empty}) parExpr

-- Semantics ------------------------------------------------------------------ -------------------

rfvSubstitute :: Expression -> String -> Expression -> Set String -> Expression
rfvSubstitute expr@(Var ename) name rexpr rfv
  | ename == name = rexpr
  | otherwise = expr
rfvSubstitute (Appl exprA exprB) name rexpr rfv =
  Appl ((rfvSubstitute exprA) name rexpr rfv)
       ((rfvSubstitute exprB) name rexpr rfv)
rfvSubstitute expr@(Abst param _) name rexpr rfv
  | param == name = expr
  | otherwise =
    let paramFree = Set.member param rfv
        alpha = alphaConvert expr (param ++ "_")
        (Abst aname abody) = if paramFree then alpha else expr
    in  Abst aname (rfvSubstitute abody name rexpr rfv)

substitute :: Expression -> String -> Expression -> Expression
substitute expr@(Appl (Abst "@b e" _) _) name rexpr =
  substitute (fullReduce expr) name rexpr
substitute expr@(Appl (Abst "@b b" _) _) name rexpr =
  substitute (fullReduce expr) name rexpr
substitute expr name rexpr@(Appl (Abst "@b b" _) _) =
  substitute expr name (fullReduce rexpr)
substitute expr name rexpr@(Appl (Abst "@b" _) _) =
  substitute expr name (fullReduce rexpr)
substitute expr name rexpr =
  rfvSubstitute expr name rexpr (freeVars rexpr)

alphaConvert :: Expression -> String -> Expression
alphaConvert (Abst name body) newName =
  Abst newName (substitute body name (Var newName))
alphaConvert expr _ = expr

betaReduce :: Expression -> (Bool, Expression)
betaReduce (Appl (Abst name body) rexpr) =
  (True, substitute body name rexpr)
betaReduce expr = (False, expr)

etaConvert :: Expression -> (Bool, Expression)
etaConvert expr@(Abst param (Appl inner (Var name)))
  | param == name && Set.notMember param (freeVars inner) = (True, inner)
  | otherwise = (False, expr)
etaConvert expr = (False, expr)

reduceExpr :: Expression -> (Bool, Expression)
reduceExpr expr@(Var name) = (False, expr)
reduceExpr expr@(Abst name body) =
  let (success, eta) = etaConvert expr
  in  if success
      then (True, eta)
      else let (was, reduced) = reduceExpr body
           in  (was, Abst name reduced)
reduceExpr expr@(Appl exprA exprB) =
  let (success, beta) = betaReduce expr
  in  if success
      then (True, beta)
      else let (wasA, reducedA) = reduceExpr exprA
           in  if wasA
               then (True, Appl reducedA exprB)
               else let (wasB, reducedB) = reduceExpr exprB
                    in  (wasB, Appl exprA reducedB)

fullReduce :: Expression -> Expression
fullReduce expr =
  let (was, reduced) = reduceExpr expr
  in  if was then fullReduce reduced else expr

roundTrip :: String -> String
roundTrip s = unparse (restoreNums (fullReduce (parse s)))

fullReduceDebug :: Expression -> Expression
fullReduceDebug expr =
  let (was, reduced) = reduceExpr expr
  in  if was then trace (unparse reduced) (fullReduce reduced) else expr

roundTripDebug :: String -> ()
roundTripDebug s = trace (unparse (restoreNums (fullReduceDebug (parse s)))) ()

-- Testing -------------------------------------------------------------------- -------------------

main :: IO ()
main = do
  args <- getArgs
  h <- if length args > 0
       then openFile ((head args) ++ ".lambd") ReadMode
       else return stdin
  contents <- hGetContents h
  parExprs <- return (fst (parseParTree (tokenize contents)))
  plIncludes <- preLoadIncludes Set.empty parExprs
  expr <- return (parseParExpr
                  (Inc {includes=plIncludes, included=Set.empty})
                  parExprs)
  putStrLn ("@@churchnums." ++ (unparse (restoreNums (fullReduce expr))))
