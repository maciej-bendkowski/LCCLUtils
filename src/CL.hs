{-# LANGUAGE BangPatterns #-}
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
module CL where
    import Memo

    -- SK CL term
    data Term =  S | K 
        | App Term Term deriving (Eq)
    
    -- pretty print    
    instance Show Term where
        show t = show' t "" where
            show' :: Term -> ShowS
            show' S = ("S" ++)
            show' K = ("K" ++)
            show' (App x  p @ (App _ _)) = show' x . ("(" ++) . show' p . (")" ++)
            show' (App x y) = show' x . show' y
   
    -- number of internal nodes
    size :: Term -> Int
    size (App x y) = (size x) + (size y) + 1
    size _ = 0
    
    -- infinite CL term list grouped by term size
    terms :: Tree [Term]
    terms = fmap (terms' g) nats where
        g :: Int -> [Term]
        g n = idx terms n
        
        terms' :: (Int -> [Term]) -> Int -> [Term]
        terms' _ 0 = [S, K]
        terms' f n = concat $ map f' $ partitions (n-1) where
            f' :: (Int, Int) -> [Term]
            f' (p, p') = [App t t' | t <- f p, t' <- f p']
            
            partitions :: Int -> [(Int, Int)]
            partitions k = [(x, y) | x <- [0..k], y <- [0..k], x + y == k]
        
    -- single CL reduction step    
    reduct :: Term -> (Term, Bool)
    reduct (App (App K x) _) = (x, True)
    reduct (App (App (App S x) y) z) = ((App (App x z) (App y z)), True)
    reduct t' = (t', False)
    
    -- head position reduction step
    headReduction :: Term -> (Term, Term, Bool)
    headReduction t @ (App x y) = case reduct t of
        (r, True) -> (r, t, True)
        (_, False) -> case headReduction x of
            (r, _, True) -> (App r y, x, True)
            (_, _, False) -> case headReduction y of
                (r, _, True) -> (App x r, y, True)
                (_, _, False) -> (t, t, False)
    headReduction x = (x, x, False)
    
    -- iterative head position reduction application 
    normalForm :: Term -> Term
    normalForm t = case headReduction t of
        (r, _, True) -> normalForm r
        (_, _, False) -> t
        
    -- iterative head position reduction application
    -- resulting in an (possibly) infinite list of intermidiate reduction terms
    normalFormReductionList :: Term -> [Term]
    normalFormReductionList t = case headReduction t of
        (r, _, True) -> r : normalFormReductionList r
        (_, _, False) -> [t]
        
    boundNormalForm :: Int -> Term -> [Term]
    boundNormalForm k t = take k $ normalFormReductionList t
    
    -- iterative head position reduction application
    -- resulting in an (possibly) infinite list of intermidiate redex terms
    normalFormRedexList :: Term -> [Term]
    normalFormRedexList t = case headReduction t of
        (r, x, True) -> x : normalFormRedexList r
        (_, x, False) -> [x]
        
    boundNormalFormRedex :: Int -> Term -> [Term]
    boundNormalFormRedex k t = take k $ normalFormRedexList t
    
    -- subterm check
    isSubterm :: Term -> Term -> Bool
    isSubterm e t @ (App t' t'')
        | e == t = True 
        | otherwise = (left || right) where
            left = e `isSubterm` t'
            right = e `isSubterm` t''
    isSubterm e t = e == t