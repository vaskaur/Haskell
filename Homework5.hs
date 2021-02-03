--Untyped Lambda Cal

data Term = Var String
        | Lambda Char Term
        | App Term Term
    deriving (Show, Eq)

--(b)
subString :: Term -> Char -> Char -> Term
subString (Var v) x y = Var (map(\i -> if i == x then y else i) v)

subst1 :: Term -> Char -> Char -> Term
subst1 (Lambda v1 (Var v2)) x y = if (subString (Var v2) x y) == (Var v2) then (Lambda v1 (Var v2)) else (Lambda v1 (subString (Var v2) x y))
subst1 (Lambda v t) x y = Lambda v (subst1 t x y)

subst2 :: Term -> Char -> Char -> Term
subst2 (Lambda v1 (Var v2)) x y = subst1 (Lambda v1 (Var v2)) x y
subst2 (Lambda v t) x y = subst1 t x y

subst :: Term -> Term -> Term
subst (Lambda v t) (Var y) = subst2 (Lambda v t) v (head y)

--(c)
isValue :: Term -> Bool
isValue (Lambda v t) = True
isValue t = False

--(d)
eval1 :: Term -> Term
eval1 (App t1 t2) = if (isValue t2) == True then (subst t1 t2) else (subst t1 (eval1 t2)) 
eval1 t = t

--(e)
eval :: Term -> Term