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
module LCTests where
    import Control.Monad (liftM2)
    import Data.Set (elems)
    import Data.Maybe (isJust, fromJust)
    import Test.QuickCheck
    import LC
    
    -- LC trees with bounded size
    arbLCTerm :: Integral a => [String] -> a -> Gen Term
    arbLCTerm [] 0 = return $ Var (head vars)
    arbLCTerm boundVars 0 = oneof [elements $ map (\s -> Var s) boundVars, return $ Var (freshVariable boundVars)]
    arbLCTerm [] n = let
        x = head vars in frequency
            [(1, return $ Var x), (5, liftM2 (App) (arbLCTerm [] (n `div` 2)) (arbLCTerm [] (n `div` 2))),
            (5, liftM2 (Abs) (return x) (arbLCTerm ([x]) (n - 1)))]
    arbLCTerm boundVars n = let
        x = freshVariable boundVars in frequency
            [(1, elements $ map (\s -> Var s) boundVars), (1, return $ Var x),
            (5, liftM2 (App) (arbLCTerm boundVars (n `div` 2)) (arbLCTerm boundVars (n `div` 2))),
            (5, liftM2 (Abs) (return x) (arbLCTerm (x : boundVars) (n - 1)))]
    
    -- random LC tree generator
    instance Arbitrary Term where
        arbitrary = sized $ arbLCTerm []
    
    -- LC-terms have non-negative size
    propLC_NonnegativeSize :: Term -> Property
    propLC_NonnegativeSize t = collect (size t) $ (size t) >= 0
    
    -- Variable rename does not affect free variables
    propLC_VarRemane :: Term -> Property
    propLC_VarRemane t = let v = boundVar t
     in collect (size t) $ isJust v && isAbs t ==> 
        freeVars (rename t (fromJust v) (freshVariable $ (fromJust v) : (elems $ freeVars t))) == (freeVars t) where
            boundVar :: Term -> Maybe String
            boundVar (Var _) = Nothing
            boundVar (App t' t'') = case boundVar t' of
                Just x -> Just x
                Nothing -> boundVar t''
            boundVar (Abs x _) = Just x
            
            isAbs :: Term -> Bool
            isAbs (Abs _ _) = True
            isAbs _ = False
            
    -- Reduction applies only to terms containing redexes
    propLC_ReductRedex :: Term -> Property
    propLC_ReductRedex t = collect (size t) $ hasRedex t ==> 
        case headReduction t of
            (_, True) -> True
            _ -> False
    
    -- Don't reduct terms without redexes
    propLC_DontReductWithoutRedex :: Term -> Property
    propLC_DontReductWithoutRedex t = collect (size t) $ (not $ hasRedex t) ==> 
        case headReduction t of
            (_, False) -> True
            _ -> False
    
    suite :: [([Char], Term -> Property)]
    suite = [("LC-terms have non-negative size", propLC_NonnegativeSize),
        ("Variable rename does not affect free variables", propLC_VarRemane),
        ("Reduction applies only to terms containing redexes", propLC_ReductRedex),
        ("Don't reduct terms without redexes", propLC_DontReductWithoutRedex)]
    
    -- test runner
    main :: IO ()
    main  = mapM_ (\(s, a) -> do
       putStr $ s ++ " "
       quickCheck a) suite