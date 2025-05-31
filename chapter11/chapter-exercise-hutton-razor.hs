-- [[file:chapter-exercises.org::hutton-one][hutton-one]]
data Expr = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add x y) = (eval x) + (eval y)

printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add x y) = printExpr x ++ " + " ++ printExpr y
-- hutton-one ends here

-- [[file:chapter-exercises.org::hutton-two][hutton-two]]
printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add x y) = printExpr x ++ " + " ++ printExpr y
-- hutton-two ends here
