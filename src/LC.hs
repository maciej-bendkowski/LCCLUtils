{-|
    Copyright (c) 2014 Maciej Bendkowski
  
    Permission is hereby granted, free of charge, to any person obtaining
    a copy of this software and associated documentation files (the
    "Software"), to deal in the Software without restriction, including
    without limitation the rights to use, copy, modify, merge, publish,
    distribute, sublicense, and/or sell copies of the Software, and to
    permit persons to whom the Software is furnished to do so, subject to
    the following conditions:
    
    The above copyright notice and this permission notice shall be
    included in all copies or substantial portions of the Software.
    
    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
    LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
    OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
    WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}
module LC where
    import Data.List (find)
    import Data.Maybe (fromJust)
    import Data.Set (Set, singleton, union, delete, notMember)
    
    -- untyped LC term
    data Term = Var String
        | App Term Term
        | Abs String Term
    
    -- variable name
    name :: Term -> String
    name (Var x) = x
    name _ = ""
    
    -- number of internal nodes
    size :: Term -> Int
    size (Var _) = 0
    size (Abs _ t) = 1 + size t
    size (App t u) = 1 + size t + size u
        
    -- pretty print    
    instance Show Term where
        show t = show' t "" where
            show' :: Term -> ShowS
            show' (Var x) = (x ++)
            show' (Abs x s) = ("\\" ++) . (x ++) . ("." ++) . show' s
            show' (App x y @ (App _ _)) = show' x . ("(" ++) . show' y . (")" ++)
            show' (App v @ (Var _) v' @ (Var _)) = show' v . show' v'
            show' (App v @ (Var _) t') = show' v . ("(" ++) . show' t' . (")" ++)
            show' (App t' v @ (Var _)) = ("(" ++) . show' t' . (")" ++) . show' v
            show' (App x y) = ("(" ++) . show' x . (")" ++) . ("(" ++) . show' y . (")" ++)
        
    -- free variables
    freeVars :: Term -> Set String
    freeVars (Var x) = singleton x
    freeVars (App t t') = freeVars t `union` freeVars t'
    freeVars (Abs x t) = delete x $ freeVars t
    
    -- infinite list of variable names
    vars :: [String]
    vars = vars' ++ (concat $ map f [1..]) where
        f :: Int -> [String]
        f n = map (\s -> s ++ show n) vars'
        
        vars' :: [String]
        vars' = [c : "" | c <- ['a'..'z']]
    
    -- rename v to z in t
    rename :: Term -> String -> String -> Term
    rename t @ (Var x) v z 
        | x == v = Var z
        | otherwise = t
    rename (App t t') v z = App (rename t v z) (rename t' v z)
    rename t @ (Abs x t') v z
        | x /= v = Abs x (rename t' v z)
        | otherwise = t
    
    -- capture-avoiding substitution
    sub :: Term -> String -> Term -> Term
    sub t @ (Var y) x r
        | y == x = r
        | otherwise = t
    sub (App t t') x r = App (sub t x r) (sub t' x r)
    sub t @ (Abs y t') x r
        | y == x = t
        | otherwise = let vs = freeVars r in
            if y `notMember` vs then Abs y (sub t' x r)
            else sub (Abs (freshVar vs) $ rename t' y (freshVar vs)) x r where
                freshVar :: Set String -> String
                freshVar s = fromJust $ find (\z -> z `notMember` s) vars
    
    -- single beta reduction step             
    reduct :: Term -> (Term, Bool)
    reduct (App (Abs x t) s) = (sub t x s, True)
    reduct t = (t, False)
    
    -- head position reduction step
    headReduction :: Term -> (Term, Bool)
    headReduction t @ (App x y) = case reduct t of
        (r, True) -> (r, True)
        (_, False) -> case headReduction x of
            (r, True) -> (App r y, True)
            (_, False) -> case headReduction y of
                (r, True) -> (App x r, True)
                (_, False) -> (t, False)
    headReduction t @ (Abs x t') = case headReduction t' of
        (r, True) -> (Abs x r, True)
        (_, False) -> (t, False)
    headReduction x = (x, False)
    
    -- iterative head position reduction application 
    normalForm :: Term -> Term
    normalForm t = case headReduction t of
        (r, True) -> normalForm r
        (_, False) -> t
        
    -- iterative head position reduction application
    -- resulting in an (possibly) infinite list of intermidiate reduction terms
    normalFormReductionList :: Term -> [Term]
    normalFormReductionList t = case headReduction t of
        (r, True) -> r : normalFormReductionList r
        (_, False) -> [t]
        
    boundNormalForm :: Int -> Term -> [Term]
    boundNormalForm k t = take k $ normalFormReductionList t