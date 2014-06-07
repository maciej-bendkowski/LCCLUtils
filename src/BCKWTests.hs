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
module BCKWTests where
    import Control.Monad (liftM2)
    import Test.QuickCheck
    import BCKW
    import Types
    
    -- BCKW trees with bounded size
    arbBCKWTerm :: Integral a => a -> Gen Term
    arbBCKWTerm 0 = oneof [return B, return C, return K, return W]
    arbBCKWTerm n = frequency
            [(1, return B), (1, return C), (1, return K), (1, return W),
            (5, liftM2 (App) (arbBCKWTerm (n `div` 2)) (arbBCKWTerm (n `div` 2)))]
    
    -- random BCKW tree generator
    instance Arbitrary Term where
        arbitrary = sized arbBCKWTerm
    
    -- BCKW-terms have non-negative size
    propBCKW_NonnegativeSize :: Term -> Property
    propBCKW_NonnegativeSize t = collect (size t) $ (size t) >= 0
    
    -- Each BCKW-term is a subterm of itself
    propBCKW_SubtermItself :: Term -> Property
    propBCKW_SubtermItself t = collect (size t) $ t `isSubterm` t
    
    -- Reduction applies only to terms containing redexes
    propBCKW_ReductRedex :: Term -> Property
    propBCKW_ReductRedex t = collect (size t) $ hasRedex t ==> 
        case headReduction t of
            (_, _, True) -> True
            _ -> False
    
    -- Don't reduct terms without redexes
    propBCKW_DontReductWithoutRedex :: Term -> Property
    propBCKW_DontReductWithoutRedex t = collect (size t) $ (not $ hasRedex t) ==> 
        case headReduction t of
            (_, _, False) -> True
            _ -> False
    
    -- full test suite
    suite :: [([Char], Term -> Property)]
    suite = [("BCKW-terms have non-negative size", propBCKW_NonnegativeSize),
        ("Each BCKW-term is a subterm of itself",  propBCKW_SubtermItself),
        ("BCKW-Reduction applies only to terms containing redexes", propBCKW_ReductRedex),
        ("Don't reduct BCKW-terms without redexes", propBCKW_DontReductWithoutRedex)]
        
    -- test runner
    main :: IO ()
    main  = mapM_ (\(s, a) -> do
       putStr $ s ++ " "
       quickCheck a) suite