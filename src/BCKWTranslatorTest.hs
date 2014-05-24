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
module BCKWTranslatorTest where
    import qualified CL
    import qualified BCKW
    import BCKWTranslator
    import Control.Monad (liftM2)
    import Test.QuickCheck
    import Types
    
    -- SK trees with bounded size
    arbSKTerm :: Integral a => a -> Gen CL.Term
    arbSKTerm 0 = oneof [return CL.K, return CL.S]
    arbSKTerm n = frequency
            [(1, return CL.S), (1, return CL.K),
            (5, liftM2 (CL.App) (arbSKTerm (n `div` 2)) (arbSKTerm (n `div` 2)))]
    
    -- random SK tree generator
    instance Arbitrary CL.Term where
        arbitrary = sized arbSKTerm
        
    -- CL to BCKW translation preserves extensional equality
    prop_CLBCKWTranslation :: CL.Term -> Property
    prop_CLBCKWTranslation t = collect (size t) $ (toBCKW t) 
        `BCKW.reducesTo` (bckwNF $ toBCKW $ clNF t) where
         
         bckwNF :: BCKW.Term -> BCKW.Term
         bckwNF t' = last $ BCKW.boundNormalForm 250 t'
         
         clNF :: CL.Term -> CL.Term
         clNF t' = last $ CL.boundNormalForm 250 t'
    
    suite :: [([Char], CL.Term -> Property)]
    suite = [("CL to BCKW translation preserves extensional equality", prop_CLBCKWTranslation)]
    
    -- test runner
    main :: IO ()
    main  = mapM_ (\(s, a) -> do
       putStr $ s ++ " "
       quickCheck a) suite