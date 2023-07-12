module Calculator where

import Control.Monad (forever)

import Data.Char (isSpace)
import Data.Function (on)
import Data.List (minimumBy)

import System.IO (hFlush, stdout)

data BinOp = Add | Sub | Mul | Div | Exp
    deriving (Show, Eq);
data Token = NumberToken Float | BinOpToken BinOp | OpenParenToken | CloseParenToken | Terminator
    deriving (Show, Eq);

data NAryTree a where
  NAryTree :: [Either a (NAryTree a)] -> NAryTree a
  deriving (Show, Eq);

type ParenthesizedTokens = NAryTree Token;

data CalculationExpr = NumberExpr Float | BinOpExpr BinOp CalculationExpr CalculationExpr
    deriving (Show, Eq);

data ParseError = InvalidToken | OpenParenNotClosed | TooManyCloseParens | EmptyTerm | ConsecutiveTerms
    deriving (Show, Eq);


-- Step 1: How do we parse a String into a list of Tokens?

charToBinOp :: Char -> Maybe BinOp
charToBinOp '+' = Just Add
charToBinOp '-' = Just Sub
charToBinOp '*' = Just Mul
charToBinOp '/' = Just Div
charToBinOp '^' = Just Exp
charToBinOp _ = Nothing

charToToken :: Char -> Maybe Token
charToToken '(' = Just OpenParenToken
charToToken ')' = Just CloseParenToken
charToToken c = BinOpToken <$> charToBinOp c

-- How do we parse strings into lists of tokens?
lexTokens :: String -> Either ParseError [Token]
lexTokens [] = Right []
lexTokens (c:cs) = case charToToken c of
    Just t -> (t :) <$> lexTokens cs
    Nothing -> if isSpace c then lexTokens cs else
        case reads (c:cs) of
            [(f, rest)] -> (NumberToken f :) <$> lexTokens rest
            _ -> Left InvalidToken


-- Step 2: How do we turn a list of tokens into a parenthetical token tree?
parenthesizeTokens :: [Token] -> Either ParseError (ParenthesizedTokens, [Token])
parenthesizeTokens [] = Right (NAryTree [], [])
parenthesizeTokens (OpenParenToken:ts) = do
    (interior, afterClose) <- parenthesizeTokens ts
    (NAryTree interior', afterClose') <- parenthesizeTokens afterClose
    Right (NAryTree (Right interior:interior'), afterClose')
parenthesizeTokens (CloseParenToken:ts) = Right (NAryTree [], ts)

parenthesizeTokens (t:ts) = do
    (NAryTree interior, afterClose) <- parenthesizeTokens ts
    Right (NAryTree (Left t:interior), afterClose)

strictParenthesizeTokens :: [Token] -> Either ParseError ParenthesizedTokens
strictParenthesizeTokens ts = do
    (tree, remainingTokens) <- parenthesizeTokens (ts ++ [CloseParenToken, Terminator])
    case remainingTokens of
        [Terminator] -> Right tree
        [] -> Left OpenParenNotClosed
        _ -> Left TooManyCloseParens

-- Step 3: How do we turn a parenthetical token tree into a calculation expression?
precedence :: BinOp -> Int
precedence Add = 1
precedence Sub = 1
precedence Mul = 2
precedence Div = 2
precedence Exp = 3

type PartlyFormedExpression = [Either CalculationExpr BinOp];
allOperatorBreaks :: PartlyFormedExpression -> [(BinOp, PartlyFormedExpression, PartlyFormedExpression)]
allOperatorBreaks [] = []
allOperatorBreaks (x:xs) = (case x of
    Left expr -> []
    Right op -> [(op, [], xs)]
    ) ++ map (\(b,l,r) -> (b,x:l,r)) (allOperatorBreaks xs)
splitAtLowestPrecedence :: PartlyFormedExpression -> Maybe (BinOp, PartlyFormedExpression, PartlyFormedExpression)
splitAtLowestPrecedence xs = let bs = (allOperatorBreaks xs) in
    if null bs then Nothing else
        Just $ minimumBy (compare `on` (\(op,_,rhs) -> (precedence op, length rhs))) bs
        -- Note: we break ties in favor of RHS being shorter to make the operations left-associative.
        -- That is, the right most operation in a tier is the one that happens
        -- last, so it should be the one we split the expression on first.

groupOperations :: PartlyFormedExpression -> Either ParseError CalculationExpr
groupOperations [] = Left EmptyTerm
groupOperations (Right Sub:xs) = groupOperations (Left (NumberExpr (-1.0)):Right Mul:xs)
groupOperations [Left expr] = Right $ expr
groupOperations xs = (case splitAtLowestPrecedence xs of
    Nothing -> Left ConsecutiveTerms
    Just (op, left, right) -> BinOpExpr op <$> groupOperations left <*> groupOperations right)

buildCalculationExpr :: ParenthesizedTokens -> Either ParseError CalculationExpr
buildCalculationExpr (NAryTree xs) = do
    subtrees <- sequence $ map handleBranch xs
    groupOperations subtrees
    where
        handleBranch :: Either Token ParenthesizedTokens -> Either ParseError (Either CalculationExpr BinOp)
        handleBranch (Left (NumberToken f)) = Right $ Left $ NumberExpr f
        handleBranch (Left (BinOpToken op)) = Right $ Right op
        handleBranch (Left OpenParenToken) = Left InvalidToken
        handleBranch (Left CloseParenToken) = Left InvalidToken
        handleBranch (Right tree) = Left <$> buildCalculationExpr tree

-- Step 4: How do we evaluate a `CalculationExpr`?
perform :: BinOp -> Float -> Float -> Float
perform Add = (+)
perform Sub = (-)
perform Mul = (*)
perform Div = (/)
perform Exp = (**)


evalCalculationExpr :: CalculationExpr -> Float
evalCalculationExpr (NumberExpr f) = f
evalCalculationExpr (BinOpExpr op e1 e2) = perform op (evalCalculationExpr e1) (evalCalculationExpr e2)


--- Put the pieces together

parseAndEvalCalculation :: String -> Either ParseError Float
parseAndEvalCalculation s = do
    tokens <- lexTokens s
    tree <- strictParenthesizeTokens tokens
    expr <- buildCalculationExpr tree
    return $ evalCalculationExpr expr

-- Interatctive loop
main :: IO ()
main = forever $ do
    putStr "> "
    hFlush stdout
    line <- getLine
    case parseAndEvalCalculation line of
        Left err -> putStrLn $ "Error: " ++ show err
        Right result -> putStrLn $ "Result: " ++ show result

