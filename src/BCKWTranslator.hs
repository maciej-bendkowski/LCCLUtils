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
module BCKWTranslator where
    import qualified CL
    import qualified BCKW
    
    toCL :: BCKW.Term -> CL.Term
    toCL BCKW.B = CL.App (CL.App CL.S (CL.App CL.K CL.S)) CL.K
    toCL BCKW.C = CL.App (CL.App CL.S ( CL.App (CL.App CL.S (CL.App CL.K 
        (CL.App (CL.App CL.S (CL.App CL.K CL.S)) CL.K))) CL.S )) (CL.App CL.K CL.K)
    toCL BCKW.W = CL.App (CL.App CL.S CL.S) (CL.App CL.S CL.K) 
    toCL BCKW.K = CL.K
    toCL (BCKW.App t t') = CL.App (toCL t) (toCL t')
    
    toBCKW :: CL.Term -> BCKW.Term
    toBCKW CL.S = BCKW.App (BCKW.App BCKW.B (BCKW.App BCKW.B BCKW.W)) (BCKW.App (BCKW.App BCKW.B BCKW.B) BCKW.C)
    toBCKW CL.K = BCKW.K
    toBCKW (CL.App t t') = BCKW.App (toBCKW t) (toBCKW t')