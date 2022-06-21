{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wall #-}
data Expr a = Lit Integer
  | Var a
  | Add (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  | Negate (Expr a)
  | Abs (Expr a)
  | Signum (Expr a)
  deriving (Eq, Ord, Show, Functor)

instance Num (Expr a) where
  e1 + e2 = Add e1 e2
  e1 * e2 = Mul e1 e2
  negate e1 = Negate e1
  abs e1 = Abs e1
  signum e1 = Signum e1
  fromInteger n = Lit n

x = Var "x"
y = Var "y"
z = Var "z"

eval :: Expr Integer -> Integer
eval (Lit n) = n
eval (Var n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Negate e1) = negate (eval e1)
eval (Abs e1) = abs (eval e1)
eval (Signum e1) = signum (eval e1)

display :: Expr String -> String
display (Lit n) = show n
display (Var s) = s
display (Add e1 e2) = display e1 ++ " + " ++ display e2
display (Mul e1 e2) = "(" ++ display e1 ++ ") * (" ++ display e2 ++ ")"
display (Negate e1) = "(-" ++ display e1 ++ ")"
display (Abs e1) = "|" ++ display e1 ++ "|"
display (Signum e1) = "signum " ++ display e1

getValueFromMap :: Eq var => [(var, out)] -> out -> var -> out
getValueFromMap [] def _ = def
getValueFromMap  ((keyH, valueH) : mapT) def key
  | keyH == key = valueH
  | otherwise = getValueFromMap mapT def key

evalWith :: Eq var => Integer -> [(var, Integer)] -> Expr var -> Integer
evalWith def mapp expr = eval (fmap (\x -> getValueFromMap mapp def x) expr) 

displayWith :: (var -> String) -> Expr var -> String
displayWith f expr = display (fmap f expr)

evalWithExample :: String
evalWithExample = show (evalWith 0 [("x", 2), ("y", 3)] (Add (Var "x") (Var "y")))

displayWithExample :: String
displayWithExample = displayWith show (Mul (Var "x") (Add (Lit 2) (Var "y")))

displayWithExample2 :: String
displayWithExample2 = displayWith display (Mul (Var (x + y)) (2 + Var (y)))

reduceUselessVars :: Expr (Expr x) -> Expr x
reduceUselessVars (Lit a) = Lit a
reduceUselessVars (Var a) = a
reduceUselessVars (Add e1 e2) = (Add (reduceUselessVars e1) (reduceUselessVars e2)) 
reduceUselessVars (Mul e1 e2) = (Mul (reduceUselessVars e1) (reduceUselessVars e2)) 
reduceUselessVars (Negate e1) = (Negate (reduceUselessVars e1)) 
reduceUselessVars (Abs e1) = (Abs (reduceUselessVars e1)) 
reduceUselessVars (Signum  e1) = (Signum (reduceUselessVars e1)) 

expandVars :: Eq var => Expr out -> [(var, Expr out)] -> Expr var -> Expr out
expandVars def mapp expr = reduceUselessVars (fmap (\x -> getValueFromMap mapp def x) expr)

expandVarsDisplayExample :: String
expandVarsDisplayExample = display (expandVars unknown vars (x * y))
  where
    unknown = Var "<unknown>"
    vars = [("x", (abs x) + z), ("y", (negate x) + 3)]

expandVarsEvalExample :: Integer
expandVarsEvalExample = eval (expandVars uninitialised intVars (((abs y) + z) * (x + 3)))
  where
    uninitialised = Negate 5
    intVars = [("x", 3), ("y", negate 4)]

main :: IO ()
main = putStrLn (show expandVarsEvalExample)
