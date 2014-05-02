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
module CLTests where
    import Control.Monad (liftM2)
    import Test.QuickCheck
    import CL
    
    -- SK trees with bounded size
    arbSKTerm :: Integral a => a -> Gen Term
    arbSKTerm 0 = oneof [return K, return S]
    arbSKTerm n = frequency
            [(1, return S), (1, return K),
            (5, liftM2 (App) (arbSKTerm (n `div` 2)) (arbSKTerm (n `div` 2)))]
    
    -- random SK tree generator
    instance Arbitrary Term where
        arbitrary = sized arbSKTerm
    
    -- SK-terms have non-negative size
    propCL_NonnegativeSize :: Term -> Property
    propCL_NonnegativeSize t = collect (size t) $ (size t) >= 0
    
    -- Each SK-term is a subterm of itself
    propCL_SubtermItself :: Term -> Property
    propCL_SubtermItself t = collect (size t) $ t `isSubterm` t
    
    -- Reduction applies only to terms containing redexes
    propCL_ReductRedex :: Term -> Property
    propCL_ReductRedex t = collect (size t) $ hasRedex t ==> 
        case headReduction t of
            (_, _, True) -> True
            _ -> False
    
    -- Don't reduct terms without redexes
    propCL_DontReductWithoutRedex :: Term -> Property
    propCL_DontReductWithoutRedex t = collect (size t) $ (not $ hasRedex t) ==> 
        case headReduction t of
            (_, _, False) -> True
            _ -> False
    
    -- full test suite
    suite :: [([Char], Term -> Property)]
    suite = [("SK-terms have non-negative size", propCL_NonnegativeSize),
        ("Each SK-term is a subterm of itself",  propCL_SubtermItself),
        ("SK-Reduction applies only to terms containing redexes", propCL_ReductRedex),
        ("Don't reduct SK-terms without redexes", propCL_DontReductWithoutRedex)]
        
    -- test runner
    main :: IO ()
    main  = mapM_ (\(s, a) -> do
       putStr $ s ++ " "
       quickCheck a) suite