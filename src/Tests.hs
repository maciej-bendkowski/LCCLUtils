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
module Tests where
    import Control.Monad (liftM2)
    import Test.QuickCheck
    import CL
    
    -- SK trees with bounded size
    arbTerm :: Integral a => a -> Gen Term
    arbTerm 0 = oneof [return K, return S]
    arbTerm n = frequency
            [(1, return S), (1, return K),
            (4, liftM2 (App) (arbTerm (n `div` 2)) (arbTerm (n `div` 2)))]
    
    -- random SK tree generator
    instance Arbitrary Term where
        arbitrary = sized arbTerm
    
    -- SK-terms have non-negative size
    prop_NonnegativeSize :: Term -> Bool
    prop_NonnegativeSize t = (size t) >=0
    
    -- Each SK-term is a subterm of itself
    prop_SubtermItself :: Term -> Bool
    prop_SubtermItself t = t `isSubterm` t
    
    -- full test suite
    suite :: [([Char], Term -> Bool)]
    suite = [("SK-terms have non-negative size", prop_NonnegativeSize),
        ("Each SK-term is a subterm of itself",  prop_SubtermItself)]
    
    -- test runner
    main :: IO ()
    main  = mapM_ (\(s, a) -> do
       putStr $ s ++ " "
       quickCheck a) suite