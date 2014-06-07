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
module BCKW where
    import Memo
    import Types
    import CommonUtils

    {-|
        Haskell Curry's BCKW system.
    -}
    data Term =  B | C | K | W 
        | App Term Term deriving (Eq)
        
    {-|
        String representation following common term notations, i.e.
        outermost parentheses are omitted, application is binding to the left.
    -}  
    instance Show Term where
        show t = show' t "" where
            show' :: Term -> ShowS
            show' B = ("B" ++)
            show' C = ("C" ++)
            show' K = ("K" ++)
            show' W = ("W" ++)
            show' (App x  p @ (App _ _)) = show' x . ("(" ++) . show' p . (")" ++)
            show' (App x y) = show' x . show' y
    
    {-|
        Infinite binary tree of BCKW combinatorial classes, i.e.
        lists of terms grouped by size.
    -}
    terms :: Tree [Term]
    terms = fmap (terms' g) nats where
        g :: Int -> [Term]
        g n = idx terms n
        
        terms' :: (Int -> [Term]) -> Int -> [Term]
        terms' _ 0 = [B, C, K, W]
        terms' f n = concat $ map f' $ partitions (n-1) where
            f' :: (Int, Int) -> [Term]
            f' (p, p') = [App t t' | t <- f p, t' <- f p']
            
            partitions :: Int -> [(Int, Int)]
            partitions k = [(x, y) | x <- [0..k], y <- [0..k], x + y == k]
    
    {-|
        Computes the number of terms of size n.
    -}
    termsOfSize :: Integer -> Integer
    termsOfSize n = (4 ^ (n + 1)) * catalan n
    
    {-|
        Common measurable BCWK term functions inherited
        from the Measurable typeclass.
    -}
    instance Measurable Term where
        
        {-|
            Number of applications in the term structure.
        -}
        size (App x y) = (size x) + (size y) + 1
        size _ = 0
        
        {-|
            Retrieves the k-th combinatorial class in O(log k) time.
        -}
        ofSize k = idx terms k
        
        {-|
            Computes the relative density of terms of size k, satisfying
            the predicate f in the overall terms of size k.
        -}
        density f k = (fromIntegral t) / (fromIntegral $ termsOfSize $ toInteger k) where
            t = length . filter (\x -> x == True) $ map f $ ofSize k
            
    {-|
        Checks if the given term is a
        primitive combinator or not.
    -}
    isCombinator :: Term -> Bool
    isCombinator B = True
    isCombinator C = True
    isCombinator K = True
    isCombinator W = True
    isCombinator _ = False
     
    {-|
        Performs a single reduction step
        to the given term if possible. Returns
        a pair (reduct, performed reduction?)
    -}    
    reduct :: Term -> (Term, Bool)
    reduct (App (App K x) _) = (x, True)
    reduct (App (App (App B x) y) z) = (App x (App y z), True)
    reduct (App (App (App C x) y) z) = (App (App x z) y, True)
    reduct (App (App W x) y) = (App (App x y) y, True)
    reduct t' = (t', False)
    
    {-|
        Performs a single head position reduction step
        to the given term if possible. Returns
        (reduct, redex term, performed reduction?)
    -}
    headReduction :: Term -> (Term, Term, Bool)
    headReduction t @ (App x y) = case reduct t of
        (r, True) -> (r, t, True)
        (_, False) -> case headReduction x of
            (r, _, True) -> (App r y, x, True)
            (_, _, False) -> case headReduction y of
                (r, _, True) -> (App x r, y, True)
                (_, _, False) -> (t, t, False)
    headReduction x = (x, x, False)
    
    {-|
        Finds the normal form a given term if possible.
        Note, this function may not terminate.
    -}
    normalForm :: Term -> Term
    normalForm t = case headReduction t of
        (r, _, True) -> normalForm r
        (_, _, False) -> t
        
    {-|
        Returns a (possibly infinite) list of intermediate
        reduction terms resulting from applying the head 
        position reduction.
    -}
    normalFormReductionList :: Term -> [Term]
    normalFormReductionList t = case headReduction t of
        (r, _, True) -> r : normalFormReductionList r
        (_, _, False) -> [t]
    
    {-|
        Returns a list of the first k intermediate
        reduction terms resulting from applying the head 
        position reduction.
    -}    
    boundNormalForm :: Int -> Term -> [Term]
    boundNormalForm k t = take k $ normalFormReductionList t
    
    {-|
        Returns a (possibly infinite) list of intermediate
        reduction redexes resulting from applying the head 
        position reduction.
    -}
    normalFormRedexList :: Term -> [Term]
    normalFormRedexList t = case headReduction t of
        (r, x, True) -> x : normalFormRedexList r
        (_, x, False) -> [x]
    
    {-|
        Returns a list of the first k intermediate
        reduction redexes resulting from applying the head 
        position reduction.
    -}    
    boundNormalFormRedex :: Int -> Term -> [Term]
    boundNormalFormRedex k t = take k $ normalFormRedexList t
    
    {-|
        Checks if e is a subterm of t.
    -}
    isSubterm :: Term -> Term -> Bool
    isSubterm e t @ (App t' t'')
        | e == t = True 
        | otherwise = (left || right) where
            left = e `isSubterm` t'
            right = e `isSubterm` t''
    isSubterm e t = e == t
    
    {-|
        Checks if the given term has a redex.
    -}
    hasRedex :: Term -> Bool
    hasRedex B = False
    hasRedex C = False
    hasRedex K = False
    hasRedex W = False
    hasRedex (App (App K _) _) = True
    hasRedex (App (App (App B _) _) _) = True
    hasRedex (App (App (App C _) _) _) = True
    hasRedex (App (App W _) _) = True
    hasRedex (App t' t'') = hasRedex t' || hasRedex t''
    
    {-|
        Checks if the given term is in normal form.
    -}
    isInNormalForm :: Term -> Bool
    isInNormalForm t = not $ hasRedex t
    
    {-|
        Checks if t reduces to r in finite reduction steps.
        Note, that this function may not terminate.
    -}
    reducesTo :: Term -> Term -> Bool
    reducesTo t r = any (r ==) $ normalFormReductionList t