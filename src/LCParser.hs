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
module LCParser where
    import Data.Char 
    import ParserUtils
    import LC
    
    -- LC grammar
    -- <variable> := [a..z] (0-9)*
    -- <abstraction> := "\" <variable> "." <term>
    -- <subterm> := <variable> | <abstraction> | "(" <term> ")"
    -- <term> := <subterm>+
    
    {-|
        Variable parser.
    -}
    variableParser :: Parser Term
    variableParser = do
        c <- charParser
        n <- token $ numberParser
        return $ Var (c : n) where
            charParser :: Parser Char
            charParser = itemPred (\s -> s `elem` ['a'..'z'])
            
            numberParser :: Parser String
            numberParser = star $ itemPred isDigit
    
    {-|
        Abstraction parser.
    -}       
    abstractionParser :: Parser Term
    abstractionParser = do
        _ <- symb "\\"
        v <- variableParser
        _ <- symb "."
        t <- termParser
        return $ Abs (name v) t
    
    {-|
        Subterm parser.
    -}     
    subtermParser :: Parser Term
    subtermParser = variableParser `dmplus` abstractionParser `dmplus` termParser' where
        termParser' :: Parser Term
        termParser' = do
            _ <- symb "("
            t <- termParser
            _ <- symb ")"
            return t
    
    {-|
        Term parser.
    -}    
    termParser :: Parser Term
    termParser = do
        ts <- pstar subtermParser
        return $ apply' (head ts) (tail ts) where
            apply' :: Term -> [Term] -> Term
            apply' t [] = t
            apply' t (t':[]) = App t t'
            apply' t (t':ts) = apply' (App t t') ts
    
    {-|
        Attempts to parse a LC term
        from the given string.
    -}            
    parseLC :: String -> Maybe Term
    parseLC s = case apply termParser s of
        (x:_) -> if null (snd x) then
             Just $ fst x else Nothing
        _ -> Nothing