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
module CLLCTranslator where
    import Data.Set (Set, singleton, union, delete, empty, notMember)
    import qualified CL 
    import qualified LC
    
    -- CL to LC translation
    toLC :: CL.Term -> LC.Term
    toLC CL.K = LC.Abs "x" $ LC.Abs "y" $ LC.Var "x"
    toLC CL.S = LC.Abs "x" $ LC.Abs "y" $ LC.Abs "z" $ 
        LC.App (LC.App (LC.Var "x") (LC.Var "z")) (LC.App (LC.Var "y") (LC.Var "z"))
    toLC (CL.App t u) = (LC.App left right) where 
        left = toLC t
        right = toLC u
    
    -- intermediate translation language
    data Term = S | K | Var String
        | App Term Term
        | Abs String Term deriving (Eq, Show)
        
    -- free variables
    freeVars :: Term -> Set String
    freeVars (Var x) = singleton x
    freeVars (App t t') = (left `union` right) where
        left = freeVars t
        right = freeVars t'
    freeVars (Abs x t) = delete x $ freeVars t
    freeVars _ = empty
    
    -- LC to intermediate language
    toInt :: LC.Term -> Term
    toInt (LC.Var x) = Var x
    toInt (LC.App x y) = (App left right) where
        left = toInt x
        right = toInt y
    toInt (LC.Abs x e) = Abs x (toInt e)
            
    -- intermediate language to CL term if possible
    fromInt :: Term -> Maybe CL.Term
    fromInt S = Just CL.S
    fromInt K = Just CL.K
    fromInt (App x y) = do
        x' <- fromInt x
        y' <- fromInt y
        return $ CL.App x' y'
    fromInt _ = Nothing
    
    -- LC to CL translation
    toCL :: LC.Term -> Maybe CL.Term
    toCL term = do
        term' <- toCL' $ toInt term
        r <- fromInt term'
        return r where
            -- abstraction elimination
            toCL' :: Term -> Maybe Term
            toCL' (App e r) = do
                e' <- toCL' e
                r' <- toCL' r
                return $ App e' r'
            toCL' (Abs x  v @ (Var y))
                | x == y = Just $ App (App S K) K
                | otherwise = Just $ App K v
            toCL' (Abs _ S) = Just $ App K S
            toCL' (Abs _ K) = Just $ App K K
            toCL' (Abs x t @ (App e r))
                | x `notMember` (freeVars t) = do
                    t' <- toCL' t
                    return $ App K t'
                | otherwise = do
                    e' <- toCL' $ Abs x e
                    r' <- toCL' $ Abs x r
                    return $ App (App S e') r'
            toCL' (Abs x t @ (Abs _ _))
                | x `notMember` (freeVars t) = do
                    t' <- toCL' t
                    return $ App K t'
                | otherwise = do
                    t' <- toCL' t
                    r <- toCL' $ Abs x t'
                    return $ r
            toCL' x = Just x