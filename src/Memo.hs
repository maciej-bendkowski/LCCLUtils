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
module Memo where
    -- open recursion memorization pattern

    -- infinite full binary tree
    data Tree a = Tree (Tree a) a (Tree a)
    instance Functor Tree where
        fmap f (Tree l x r) = Tree (fmap f l) (f x) (fmap f r)
    
    -- tree indexer
    idx :: Tree a -> Int -> a
    idx (Tree _ x _) 0 = x
    idx (Tree l _ r) k = case (k-1) `divMod` 2 of
        (k', 0) -> idx l k'
        (k', _) -> idx r k' 

    -- natural numbers
    nats :: Tree Int
    nats = f 0 1 where
        f :: Int -> Int -> Tree Int
        f !k !s = Tree (f l s') k (f r s') where
            l = k + s
            r = l + s
            s' = s * 2
    
    -- converts to list
    toList :: Tree a -> [a]
    toList ts = map (idx ts) [0..]