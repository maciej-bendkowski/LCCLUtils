{-# LANGUAGE DoAndIfThenElse #-}
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
module ParserUtils where
    import Control.Monad
    import Data.Char
    
    {-|
        Recursive descent parser type definition.
    -}
    newtype Parser a = Parser (String -> [(a, String)])
    parse :: Parser a -> String -> [(a, String)]
    parse (Parser p) = p
    
    {-|
        Monadic parser definition.
    -}
    instance Monad Parser where
        return x = Parser $ \s -> [(x, s)]
        p >>= f = Parser $ \s -> concat [ parse (f x) s' | (x, s') <- parse p s]
        
    instance MonadPlus Parser where
        mzero = Parser $ const []
        p `mplus` q = Parser $ \s -> parse p s ++ parse q s
        
    {-|
        Deterministic choice argument
    -}
    dmplus :: Parser a -> Parser a -> Parser a
    p `dmplus` q = Parser $ \s -> case parse (p `mplus` q) s of
        (x:_) -> [x]
        _ -> []
    
    {-|
        Single char parser
    -}    
    item :: Parser Char
    item = Parser $ \s -> case s of
        x:xs -> [(x, xs)]
        _ -> []
    
    {-|
        Single char predicate parser
    -}      
    itemPred :: (Char -> Bool) -> Parser Char
    itemPred p = do
        x <- item
        if p x then return x
        else mzero
    
    {-|
        Single functional char parser
    -} 
    char :: Char -> Parser Char
    char x = itemPred (x ==)
    
    {-|
        Single functional string parser
    -}
    str :: String -> Parser String
    str "" = return ""
    str (x:xs) = do
        _ <- char x
        _ <- str xs
        return (x:xs)
        
    {-|
        Kleene star operator
    -} 
    star :: Parser a -> Parser [a]
    star p = pstar p `dmplus` return [] 
    
    {-|
        Kleene plus operator
    -} 
    pstar :: Parser a -> Parser [a]
    pstar p = do
        x <- p
        xs <- star p
        return (x:xs)
        
    {-|
        Whitespace parser
    -} 
    space :: Parser String
    space = star $ itemPred isSpace
    
    {-|
        Token parser removing trailing space.
    -} 
    token :: Parser a -> Parser a
    token p = do
        x <- p
        _ <- space
        return x
    
    {-|
        Symbolic token parser
    -}   
    symb :: String -> Parser String
    symb s = token $ str s
    
    {-|
        Applies a parser removing leading whitespace.
    -} 
    apply :: Parser a -> String -> [(a, String)]
    apply p = parse (do { _ <- space; p })