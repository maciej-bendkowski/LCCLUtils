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
module TranslatorTest where
    import Control.Monad (liftM2)
    import Data.Set (null)
    import Test.QuickCheck
    import LC
    
    -- Closed LC trees with bounded size
    arbLCTerm :: Integral a => a -> Gen LC.Term
    arbLCTerm n = let
            x = head vars in liftM2 (LC.Abs) (return x) (arbLCTerm' [x] n) where
        arbLCTerm' :: Integral a => [String] -> a -> Gen LC.Term
        arbLCTerm' boundVars 0 = elements $ map (\s -> LC.Var s) boundVars
        arbLCTerm' boundVars k = let
            x = freshVariable boundVars in frequency
                [(1, elements $ map (\s -> LC.Var s) boundVars),
                (3, liftM2 (LC.App) (arbLCTerm' boundVars (k `div` 2)) (arbLCTerm' boundVars (k `div` 2))),
                (3, liftM2 (LC.Abs) (return x) (arbLCTerm' (x : boundVars) (k - 1)))]
    
    -- random LC tree generator
    instance Arbitrary LC.Term where
        arbitrary = sized $ arbLCTerm
    
    -- generated LC terms are closed
    prop_ClosedTerms :: LC.Term -> Property
    prop_ClosedTerms t = collect (LC.size t) $ Data.Set.null (LC.freeVars t)
    
    suite :: [([Char], LC.Term -> Property)]
    suite = [("Generated LC terms are closed", prop_ClosedTerms)]
    
    -- test runner
    main :: IO ()
    main  = mapM_ (\(s, a) -> do
       putStr $ s ++ " "
       quickCheck a) suite