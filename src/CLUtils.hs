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
module CLUtils where
    import Data.List
    import Types
    import CL
    import CLParser
    import Data.Maybe
    
    {-|
        Finds candidates of size k, that do not admit a normal form
        after performing n reduction steps.
    -}
    findCandidates :: Int -> Int -> [Term]
    findCandidates k n = map (\x -> ofSize k !! (fst x)) $ filter (\x -> snd x == n) $ zip [0..]
        (map (\x -> length $ boundNormalForm n x) $ ofSize k)
    
    {-|
        Finds candidates of size k, that do not admit a normal form
        after performing n reduction steps and moreover do not contain
        candidates of size k - 1 as subterms.
    -}
    extract :: Int -> Int -> [Term]
    extract k n = filter (\y -> all (\x -> not $ isSubterm x y) xs) ys where
        xs = findCandidates (k - 1) n
        ys = findCandidates k n
    
    {-|
       Normalizes all terms of size k or less, with the
       exception of the given term list.
    -}
    normalize :: Int -> [Term] -> [Term]
    normalize k ts = map normalForm $ (tms \\ ts) where
        tms = concat . take (k + 1) $ map ofSize [0..]
    
    {-|
        List of two S-terms of size 6 without normal forms.
    -}    
    leastWithoutNF :: [Term]
    leastWithoutNF = [fromJust $ parseCL "SSS(SS)SS", fromJust $ parseCL "S(SS)SSSS"]