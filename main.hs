main :: IO ()
main = do
  let file = "tekst.txt"
  script <- readFile file
  let x = nest (tokenize (script ))
  functionIO (x,[],x,NSymbol "START","")
  --let e = parse (x,[],x)
  --let e = nest (tokenize (script ))
  --let e = tokenize (script )
  --print e
  return ()
  
functionIO :: (N,[(String,Float)],N,N,String) -> IO ()
functionIO (n,values,k,NSymbol "START",s) = functionIO(parse(n,values,k))
functionIO (n,values,k,NSymbol "START1",s) = print n
functionIO (n,values,k,NSymbol "READ",s) = do
  x <- getLine
  t <- getChar
  let z = if isFirstDot (dropWhile isNumber x) then NFloat (read ((takeWhile isNumber x) ++ "." ++ (takeWhile isNumber (deleteFirstChar (dropWhile isNumber x))))) else NFloat (read ((takeWhile isNumber x))) 
  let y = toFloat z
  functionIO(parse(n,addValue(s,y,values,[]),k))
functionIO ((NList l),values,k,NSymbol "READ2",s) = do 
  x <- getLine
  t <- getChar
  let z = if isFirstDot (dropWhile isNumber x) then NFloat (read ((takeWhile isNumber x) ++ "." ++ (takeWhile isNumber (deleteFirstChar (dropWhile isNumber x))))) else NFloat (read ((takeWhile isNumber x))) 
  let y = toFloat z
  functionIO(parse((NList ((NSymbol "READ"):l)),addValue(s,y,values,[]),k))
functionIO (n,values,k,NSymbol "WRITE",s) = do 
                                            print s
                                            functionIO(parse(n,values,k))
functionIO (NEndLine,values,k,x,"") = return()
functionIO (n,values,k,NSymbol "DO",s) = functionForDOIO((n,values,k,NSymbol "START",s),"",0,0,0,n,0)


functionForDOIO :: ((N,[(String,Float)],N,N,String) ,String,Float,Float,Float,N,Float) -> IO ()
functionForDOIO (((NList ((NFloat s):(NSymbol x):(NSymbol "="):(NFloat a):(NFloat b):(NFloat c):xs)),values,k,NSymbol "START",s1),s2,a1,a2,a3,NList z,y) = if a <= b then functionForDOIO(parse((cut(NList xs,s,[])),addValue(x,a,values,[]),k),x,a+c,b,c,NList z,s) else functionIO((goToNextLine(goto(NList xs,s)),values,k,NSymbol "START",""))
functionForDOIO (((NList ((NFloat s):(NSymbol x):(NSymbol "="):(NFloat a):(NFloat b):xs)),values,k,NSymbol "START",s1),s2,a1,a2,a3,NList z,y) = if a <= b then functionForDOIO(parse((cut(NList xs,s,[])),addValue(x,a,values,[]),k),x,a+1,b,1,NList z,s) else functionIO((goToNextLine(goto(NList xs,s)),values,k,NSymbol "START",""))

functionForDOIO (((NList ((NFloat s):(NSymbol x):(NSymbol "="):(NFloat a):(NFloat b):(NFloat c):xs)),values,k,NSymbol "NEXT",s1),s2,a1,a2,a3,NList z,y) = if a1 <= b then functionForDOIO(parse((cut(NList xs,s,[])),addValue(x,a1,values,[]),k),x,a1+c,b,c,NList z,s) else functionIO((goToNextLine(goto(NList xs,s)),values,k,NSymbol "START",""))
functionForDOIO (((NList ((NFloat s):(NSymbol x):(NSymbol "="):(NFloat a):(NFloat b):xs)),values,k,NSymbol "NEXT",s1),s2,a1,a2,a3,NList z,y) = if a1 <= b then functionForDOIO(parse((cut(NList xs,s,[])),addValue(x,a1,values,[]),k),x,a1+1,b,1,NList z,s) else functionIO((goToNextLine(goto(NList xs,s)),values,k,NSymbol "START",""))

functionForDOIO ((n,values,k,NSymbol "WRITE",s1),x,a,b,c,NList z,number) = do
  print s1
  functionForDOIO (parse(n,values,k),x,a,b,c,NList z,number)
functionForDOIO ((n,values,k,NSymbol "READ",s),x,a,b,c,NList z,number) = do
  x <- getLine
  t <- getChar
  let p = if isFirstDot (dropWhile isNumber x) then NFloat (read ((takeWhile isNumber x) ++ "." ++ (takeWhile isNumber (deleteFirstChar (dropWhile isNumber x))))) else NFloat (read ((takeWhile isNumber x))) 
  let y = toFloat p
  functionForDOIO(parse(n,addValue(s,y,values,[]),k),x,a,b,c,NList z,number)
functionForDOIO (((NList l),values,k,NSymbol "READ2",s),x,a,b,c,NList z,number) = do 
  x <- getLine
  t <- getChar
  let p = if isFirstDot (dropWhile isNumber x) then NFloat (read ((takeWhile isNumber x) ++ "." ++ (takeWhile isNumber (deleteFirstChar (dropWhile isNumber x))))) else NFloat (read ((takeWhile isNumber x))) 
  let y = toFloat p
  functionForDOIO(parse((NList ((NSymbol "READ"):l)),addValue(s,y,values,[]),k),x,a,b,c,NList z,number)

functionForDOIO ((NEndLine,values,k,n,s1),x,a,b,c,NList z,number) = if a <= b then functionForDOIO((NList z,values,k,NSymbol "NEXT",s1),x,a,b,c,NList z,number) else functionIO((goToNextLine(goto(k,number)),values,k,NSymbol "START",""))



data T
  = TOpen
  | TClose
  | TSymbol String
  | TInt Int
  | TFloat Float
  | TString String
  | TEndLine

toStringFromT :: T -> [Char]
toStringFromT TOpen = "open"
toStringFromT TClose = "close"
toStringFromT (TSymbol xs) = xs
toStringFromT (TInt xs) = show xs
toStringFromT (TString xs) = xs
toStringFromT (TFloat xs) = show xs
toStringFromT (TEndLine) = "EndLine"

instance Show T where
  show x = toStringFromT x

data N
  = NList [N]
  | NSymbol String
  | NInt Int
  | NFloat Float
  | NString String
  | NEndLine

toStringFromN :: N -> String
toStringFromN (NList ns) = "NList [" ++ (toStringFromListN ns) ++ "]" 
toStringFromN (NSymbol n) = "NSymbol " ++ n ++ " "
toStringFromN (NInt n) = "NInt " ++ show n ++ " "
toStringFromN (NString n) = "NString " ++ n ++ " "
toStringFromN (NFloat n) = "NFloat " ++ show n ++ " "
toStringFromN (NEndLine) = "NEndLine "

toStringFromListN :: [N] -> String
toStringFromListN (n:[]) = toStringFromN n
toStringFromListN (n:ns) =  (toStringFromN n) ++ ", " ++ (toStringFromListN ns)

instance Show N where
  show x = toStringFromN x


isSpace :: Char -> Bool
isSpace c = if c == ' ' then True else False 

isNumber :: Char -> Bool
isNumber c = if c >= '0' && c <= '9' then True else False 

isLetter :: Char -> Bool
isLetter c = if ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') then True else False

isAlphaNum :: Char -> Bool
isAlphaNum c = if (isNumber c || isLetter c) then True else False

isSymbolChar :: Char -> Bool
isSymbolChar c = isAlphaNum c || elem c "=+*<-/%"

isNotQuotationMark :: Char -> Bool
isNotQuotationMark x = if x == '\"' then False else True

deleteFirstChar :: [Char] -> [Char]
deleteFirstChar (x:[]) = []
deleteFirstChar (x:xs) = xs 

isFirstDot :: [Char] -> Bool
isFirstDot (x:xs) = if x == '.' then True else False
isFirstDot [] = False

tokenize :: [Char] -> [T]
tokenize [] = []
tokenize ('(':cs) = TOpen : tokenize cs
tokenize (')':cs) = TClose : tokenize cs
tokenize ('\n':cs) = TEndLine : tokenize cs
tokenize ('\'':c:'\'':cs) = TInt (fromEnum c) : tokenize cs
tokenize ('\"':cs) = if (dropWhile isNotQuotationMark cs) == ['\"'] then [TString (takeWhile isNotQuotationMark cs)]  
  else (TString (takeWhile isNotQuotationMark cs)) : tokenize (deleteFirstChar(dropWhile isNotQuotationMark cs))
tokenize (c : cs)
  | isNumber c = if isFirstDot (dropWhile isNumber cs) then TFloat (read (c:(takeWhile isNumber cs) ++ "." ++ (takeWhile isNumber (deleteFirstChar (dropWhile isNumber cs))))) : tokenize (dropWhile isNumber (deleteFirstChar (dropWhile isNumber cs))) else TFloat (read (c:(takeWhile isNumber cs))) : tokenize (dropWhile isNumber cs)
  | isSpace c = tokenize cs
  | isSymbolChar c = TSymbol (c : takeWhile isSymbolChar cs) : tokenize (dropWhile isSymbolChar cs)
  | otherwise = tokenize cs
  


nestOne :: [T] -> ([N], [T])
nestOne []               = ([], [])
nestOne (TOpen     : ts) = let (ns, ts') = nestMany [] ts in ([NList ns], ts')
nestOne (TSymbol s : ts) = ([NSymbol s], ts)
nestOne (TInt i    : ts) = ([NInt i], ts)
nestOne (TString s : ts) = ([NString s], ts) 
nestOne (TFloat f  : ts) = ([NFloat f], ts)
nestOne (TEndLine  : ts) = ([NEndLine], ts)
nestOne (TClose    : ts) = ([], ts)

nestMany :: [N] -> [T] -> ([N], [T])
nestMany prev ts = case nestOne ts of
  ([], ts') -> (prev , ts')
  (ns, ts') -> nestMany (prev++ns) ts'

nest :: [T] -> N
nest ts = case nestMany [] ts of
  (ns, []) -> NList ns
  _ -> error "unexpected content"

joinlists :: ([a],[a]) -> [a]
joinlists ([],y) = y
joinlists (x:xs,y) = joinlists (xs,(x:y))

addValue :: (String,Float,[(String,Float)],[(String,Float)]) -> [(String,Float)]
addValue (x,v,[],rest) = (x,v):rest 
addValue (x,v,(x1,v1):values,rest) = if x == x1 then joinlists (values,(x1,v):rest) else addValue (x,v,values,((x1,v1):rest))  

getValue :: (String,[(String,Float)]) -> Float
getValue (x,[]) = error "incorrect temp"
getValue (x,((x1,v1):values)) = if x == x1 then v1 else getValue(x,values)

calc :: (N,[(String,Float)]) -> (Float,N)
calc ((NList ((NList l):xs)),values) = let (v,n) = calc(NList l,values) in calc(NList ((NFloat v):xs),values)
calc ((NList ((NFloat a):[])),values) = (a,NEndLine)
calc ((NList ((NFloat a):(NEndLine):xs)),values) = (a,NList xs)
calc ((NList ((NSymbol a):[])),values) = (getValue(a,values),NEndLine)
calc ((NList ((NSymbol a):(NEndLine):xs)),values) = (getValue(a,values),NList xs)
calc ((NList ((NSymbol "-"):(NSymbol a):[])),values) = (-getValue(a,values),NEndLine)
calc ((NList ((NSymbol "-"):(NFloat a):[])),values) = (-a,NEndLine)
calc ((NList ((NSymbol "-"):(NList a):[])),values) = let(v,n)= calc(NList a,values) in(-v,NEndLine)
calc ((NList ((NSymbol "-"):(NSymbol a):(NEndLine):xs)),values) = (-getValue(a,values),NList xs)
calc ((NList ((NSymbol "-"):(NFloat a):(NEndLine):xs)),values) = (-a,NList xs)
calc ((NList ((NSymbol "-"):(NList a):(NEndLine):xs)),values) = let(v,n)= calc(NList a,values) in (-v,NList xs)
calc ((NList ((NSymbol "-"):(NSymbol a):xs)),values) = calc(NList ((NFloat (-getValue(a,values))):xs),values)
calc ((NList ((NSymbol "-"):(NFloat a):xs)),values) = calc(NList ((NFloat (-a)):xs),values)
calc ((NList ((NSymbol "-"):(NList a):xs)),values) = let(v,n)= calc(NList a,values) in calc(NList ((NFloat (-v)):xs),values)
calc ((NList ((NFloat a):(NSymbol "+"):(NFloat b):[])),values) = ((a+b),NEndLine)
calc ((NList ((NSymbol a):(NSymbol "+"):(NFloat b):[])),values) = ((getValue(a,values)+b),NEndLine)
calc ((NList ((NFloat a):(NSymbol "+"):(NSymbol b):[])),values) = ((a+getValue(b,values)),NEndLine)
calc ((NList ((NSymbol a):(NSymbol "+"):(NSymbol b):[])),values) = ((getValue(a,values)+getValue(b,values)),NEndLine)
calc ((NList ((NFloat a):(NSymbol "+"):(NFloat b):(NEndLine):xs)),values) = ((a+b),NList xs)
calc ((NList ((NSymbol a):(NSymbol "+"):(NFloat b):(NEndLine):xs)),values) = ((getValue(a,values)+b),NList xs)
calc ((NList ((NFloat a):(NSymbol "+"):(NSymbol b):(NEndLine):xs)),values) = ((a+getValue(b,values)),NList xs)
calc ((NList ((NSymbol a):(NSymbol "+"):(NSymbol b):(NEndLine):xs)),values) = (((getValue(a,values)+getValue(b,values))),NList xs)
calc ((NList ((NFloat a):(NSymbol "+"):(NFloat b):xs)),values) = calc(NList ((NFloat (a+b)):xs),values)
calc ((NList ((NSymbol a):(NSymbol "+"):(NFloat b):xs)),values) = calc(NList ((NFloat (getValue(a,values)+b)):xs),values)
calc ((NList ((NFloat a):(NSymbol "+"):(NSymbol b):xs)),values) = calc(NList ((NFloat (a+getValue(b,values))):xs),values)
calc ((NList ((NSymbol a):(NSymbol "+"):(NSymbol b):xs)),values) = calc(NList ((NFloat (getValue(a,values)+getValue(b,values))):xs),values)
calc ((NList ((NSymbol a):(NSymbol "+"):(NList l):xs)),values) = let (v,n) = calc(NList l,values) in calc(NList ((NFloat (getValue(a,values)+v)):xs),values)
calc ((NList ((NFloat a):(NSymbol "+"):(NList l):xs)),values) = let (v,n) = calc(NList l,values) in calc(NList ((NFloat (a+v)):xs),values) 

calc ((NList ((NFloat a):(NSymbol "-"):(NFloat b):[])),values) = ((a-b),NEndLine)
calc ((NList ((NSymbol a):(NSymbol "-"):(NFloat b):[])),values) = ((getValue(a,values)-b),NEndLine)
calc ((NList ((NFloat a):(NSymbol "-"):(NSymbol b):[])),values) = ((a-getValue(b,values)),NEndLine)
calc ((NList ((NSymbol a):(NSymbol "-"):(NSymbol b):[])),values) = ((getValue(a,values)-getValue(b,values)),NEndLine)
calc ((NList ((NFloat a):(NSymbol "-"):(NFloat b):(NEndLine):xs)),values) = ((a-b),NList xs)
calc ((NList ((NSymbol a):(NSymbol "-"):(NFloat b):(NEndLine):xs)),values) = ((getValue(a,values)-b),NList xs)
calc ((NList ((NFloat a):(NSymbol "-"):(NSymbol b):(NEndLine):xs)),values) = ((a-getValue(b,values)),NList xs)
calc ((NList ((NSymbol a):(NSymbol "-"):(NSymbol b):(NEndLine):xs)),values) = (((getValue(a,values)-getValue(b,values))),NList xs)
calc ((NList ((NFloat a):(NSymbol "-"):(NFloat b):xs)),values) = calc(NList ((NFloat (a-b)):xs),values)
calc ((NList ((NSymbol a):(NSymbol "-"):(NFloat b):xs)),values) = calc(NList ((NFloat (getValue(a,values)-b)):xs),values)
calc ((NList ((NFloat a):(NSymbol "-"):(NSymbol b):xs)),values) = calc(NList ((NFloat (a-getValue(b,values))):xs),values)
calc ((NList ((NSymbol a):(NSymbol "-"):(NSymbol b):xs)),values) = calc(NList ((NFloat (getValue(a,values)-getValue(b,values))):xs),values)
calc ((NList ((NSymbol a):(NSymbol "-"):(NList l):xs)),values) = let (v,n) = calc(NList l,values) in calc(NList ((NFloat (getValue(a,values)-v)):xs),values)
calc ((NList ((NFloat a):(NSymbol "-"):(NList l):xs)),values) = let (v,n) = calc(NList l,values) in calc(NList ((NFloat (a-v)):xs),values)

calc ((NList ((NFloat a):(NSymbol "*"):(NFloat b):[])),values) = ((a*b),NEndLine)
calc ((NList ((NSymbol a):(NSymbol "*"):(NFloat b):[])),values) = ((getValue(a,values)*b),NEndLine)
calc ((NList ((NFloat a):(NSymbol "*"):(NSymbol b):[])),values) = ((a*getValue(b,values)),NEndLine)
calc ((NList ((NSymbol a):(NSymbol "*"):(NSymbol b):[])),values) = ((getValue(a,values)*getValue(b,values)),NEndLine)
calc ((NList ((NFloat a):(NSymbol "*"):(NFloat b):(NEndLine):xs)),values) = ((a*b),NList xs)
calc ((NList ((NSymbol a):(NSymbol "*"):(NFloat b):(NEndLine):xs)),values) = ((getValue(a,values)*b),NList xs)
calc ((NList ((NFloat a):(NSymbol "*"):(NSymbol b):(NEndLine):xs)),values) = ((a*getValue(b,values)),NList xs)
calc ((NList ((NSymbol a):(NSymbol "*"):(NSymbol b):(NEndLine):xs)),values) = (((getValue(a,values)*getValue(b,values))),NList xs)
calc ((NList ((NFloat a):(NSymbol "*"):(NFloat b):xs)),values) = calc(NList ((NFloat (a*b)):xs),values)
calc ((NList ((NSymbol a):(NSymbol "*"):(NFloat b):xs)),values) = calc(NList ((NFloat (getValue(a,values)*b)):xs),values)
calc ((NList ((NFloat a):(NSymbol "*"):(NSymbol b):xs)),values) = calc(NList ((NFloat (a*getValue(b,values))):xs),values)
calc ((NList ((NSymbol a):(NSymbol "*"):(NSymbol b):xs)),values) = calc(NList ((NFloat (getValue(a,values)*getValue(b,values))):xs),values)
calc ((NList ((NSymbol a):(NSymbol "*"):(NList l):xs)),values) = let (v,n) = calc(NList l,values) in calc(NList ((NFloat (getValue(a,values)*v)):xs),values)
calc ((NList ((NFloat a):(NSymbol "*"):(NList l):xs)),values) = let (v,n) = calc(NList l,values) in calc(NList ((NFloat (a*v)):xs),values)

calc ((NList ((NFloat a):(NSymbol "/"):(NFloat b):[])),values) = ((a/b),NEndLine)
calc ((NList ((NSymbol a):(NSymbol "/"):(NFloat b):[])),values) = ((getValue(a,values)/b),NEndLine)
calc ((NList ((NFloat a):(NSymbol "/"):(NSymbol b):[])),values) = ((a/getValue(b,values)),NEndLine)
calc ((NList ((NSymbol a):(NSymbol "/"):(NSymbol b):[])),values) = ((getValue(a,values)/getValue(b,values)),NEndLine)
calc ((NList ((NFloat a):(NSymbol "/"):(NFloat b):(NEndLine):xs)),values) = ((a/b),NList xs)
calc ((NList ((NSymbol a):(NSymbol "/"):(NFloat b):(NEndLine):xs)),values) = ((getValue(a,values)/b),NList xs)
calc ((NList ((NFloat a):(NSymbol "/"):(NSymbol b):(NEndLine):xs)),values) = ((a/getValue(b,values)),NList xs)
calc ((NList ((NSymbol a):(NSymbol "/"):(NSymbol b):(NEndLine):xs)),values) = (((getValue(a,values)/getValue(b,values))),NList xs)
calc ((NList ((NFloat a):(NSymbol "/"):(NFloat b):xs)),values) = calc(NList ((NFloat (a/b)):xs),values)
calc ((NList ((NSymbol a):(NSymbol "/"):(NFloat b):xs)),values) = calc(NList ((NFloat (getValue(a,values)/b)):xs),values)
calc ((NList ((NFloat a):(NSymbol "/"):(NSymbol b):xs)),values) = calc(NList ((NFloat (a/getValue(b,values))):xs),values)
calc ((NList ((NSymbol a):(NSymbol "/"):(NSymbol b):xs)),values) = calc(NList ((NFloat (getValue(a,values)/getValue(b,values))):xs),values)
calc ((NList ((NSymbol a):(NSymbol "/"):(NList l):xs)),values) = let (v,n) = calc(NList l,values) in calc(NList ((NFloat (getValue(a,values)/v)):xs),values)
calc ((NList ((NFloat a):(NSymbol "/"):(NList l):xs)),values) = let (v,n) = calc(NList l,values) in calc(NList ((NFloat (a/v)):xs),values)

goToNextLine :: N -> N
goToNextLine (NList (NEndLine:xs)) = (NList xs)
goToNextLine (NList (x:xs)) = goToNextLine (NList xs)
goToNextLine _ = NEndLine

goto :: (N,Float) -> N
goto ((NList ((NFloat x):xs)),v) = if x == v then NList xs else goto (goToNextLine(NList xs),v)
goto ((NList xs),v) = goto(goToNextLine(NList xs),v)
goto _ = error "wrong goto value" 

cutToNextLine :: ([N],[N]) -> [N]
cutToNextLine ([],w) = reverse w
cutToNextLine (((NEndLine):xs),w) = reverse (NEndLine:w)
cutToNextLine ((x:xs),w) = cutToNextLine (xs,(x:w)) 

cut :: (N,Float,[N]) -> N
cut ((NList ((NEndLine):(NFloat x):xs)),v,s) = if x == v then (NList (reverse (joinlists (cutToNextLine(xs,[]),(NFloat x):(NEndLine):s)))) else cut(NList xs,v,((NFloat x):(NEndLine):s))
cut ((NList (x:xs)),v,s) = cut(NList xs,v,(x:s))   

changeValues :: (N,[(String,Float)],N,String,Float) -> [(String,Float)]
changeValues  ((NList ((NSymbol x):(NSymbol "="):(NFloat a):(NEndLine):xs)),values,k,i,value) = changeValues (NList xs,addValue (x,a,values,[]),k,i,value)
changeValues ((NList ((NSymbol x):(NSymbol "="):(NSymbol a):(NEndLine):xs)),values,k,i,value) = if a == i then changeValues (NList xs,addValue (x,value,values,[]),k,i,value) else changeValues (NList xs,addValue (x,getValue(a,values),values,[]),k,i,value)
changeValues ((NList ((NSymbol x):(NSymbol "="):(NSymbol "-"):(NFloat a):(NEndLine):xs)),values,k,i,value) = changeValues (NList xs,addValue (x,(- a),values,[]),k,i,value)
changeValues ((NList ((NSymbol x):(NSymbol "="):(NSymbol "-"):(NSymbol a):(NEndLine):xs)),values,k,i,value) = if a == i then changeValues (NList xs,addValue (x,(- value),values,[]),k,i,value) else changeValues (NList xs,addValue (x,(- getValue(a,values)),values,[]),k,i,value)
changeValues ((NList ((NSymbol x):(NSymbol "="):xs)),values,k,i,value) = let (v,n) = calc(NList xs,values) in changeValues (n,addValue (x,v,values,[]),k,i,value)
changeValues  ((NList (xs)),values,k,i,value) = changeValues (goToNextLine(NList xs),values,k,i,value)
changeValues (_,values,k,i,value) = values

--loop :: (N,String,Float,Float,Float,[(String,Float)],String) -> ([(String,Float)],String)
--loop (xs,x,a,b,c,values,w) = if a >= b then (changeValues(xs,values,xs,x,a),w ++ parse(xs,addValue(x,a,values,[]),xs)) else loop (xs,x,a+c,b,c,changeValues(xs,values,xs,x,a),(w ++ parse(xs,addValue(x,a,values,[]),xs)))
        
toFloat :: N -> Float
toFloat (NFloat x) = x

parse :: (N,[(String,Float)],N) -> (N,[(String,Float)],N,N,String)
parse (NEndLine,values,k) = (NEndLine,values,k,NSymbol "END","")
parse ((NList ((NFloat x):xs)),values,k) = parse (goToNextLine (NList xs),values,k)
parse ((NList ((NEndLine):xs)),values,k) = parse (NList xs,values,k)
--parse ((NList [NSymbol "WRITE", NInt a]),values,k) = [(toEnum a::Char)]
parse ((NList [NSymbol "WRITE", NString a]),values,k) = (NEndLine,values,k,NSymbol "WRITE",a)
parse ((NList (NSymbol "WRITE": NString a:(NEndLine):xs)),values,k) = (NList xs,values,k,NSymbol "WRITE",a)
parse ((NList ((NSymbol "WRITE"):(NString a):xs)),values,k) = let (a1,a2,a3,a4,s) = parse ((NList ((NSymbol "WRITE"):xs)),values,k) in (a1,a2,a3,a4,(a ++ s))
parse ((NList [NSymbol "WRITE", NSymbol a]),values,k) = (NEndLine,values,k,NSymbol "WRITE",show (getValue (a,values))) 
parse ((NList ((NSymbol "WRITE"):(NSymbol a):(NEndLine):xs)),values,k) = (NList xs,values,k,NSymbol "WRITE",show (getValue (a,values)))  
parse ((NList ((NSymbol "WRITE"):(NSymbol a):xs)),values,k) = let (a1,a2,a3,a4,s) = parse ((NList ((NSymbol "WRITE"):xs)),values,k) in (a1,a2,a3,a4,show (getValue (a,values)) ++ s)   
--parse ((NList [NFloat a, NSymbol "+", NFloat b]),values,k) = show (a + b)
--parse ((NList ((NFloat a): (NSymbol "+"): (NFloat b):x)),values,k) = parse ((NList (NFloat (a+b):x)),values,k)
parse ((NList ((NSymbol x):(NSymbol "="):(NFloat a):(NEndLine):xs)),values,k) = parse (NList xs,addValue (x,a,values,[]),k)
parse ((NList ((NSymbol x):(NSymbol "="):(NSymbol a):(NEndLine):xs)),values,k) = parse (NList xs,addValue (x,getValue(a,values),values,[]),k)
parse ((NList ((NSymbol x):(NSymbol "="):(NSymbol "-"):(NFloat a):(NEndLine):xs)),values,k) = parse (NList xs,addValue (x,(- a),values,[]),k)
parse ((NList ((NSymbol x):(NSymbol "="):(NSymbol "-"):(NSymbol a):(NEndLine):xs)),values,k) = parse (NList xs,addValue (x,(- getValue(a,values)),values,[]),k)
parse ((NList ((NSymbol x):(NSymbol "="):xs)),values,k) = let (v,n) = calc(NList xs,values) in parse (n,addValue (x,v,values,[]),k)
parse ((NList ((NSymbol "GOTO"):(NFloat a):xs)),values,k) =  parse (goto (k,a),values,k)
parse ((NList ((NSymbol "IF"):(NList l):(NFloat a):(NFloat b):(NFloat c):xs)),values,k) = let (v,n) = calc(NList l,values) in  if v < 0 then parse (goto (k,a),values,k) else if v == 0 then parse (goto (k,b),values,k) else parse (goto (k,c),values,k)
--parse ((NList ((NSymbol "DO"):(NFloat s):(NSymbol x):(NSymbol "="):(NFloat a):(NFloat b):(NFloat c):xs)),values,k) = let (v,w) = loop(cut(NList xs,s,[]),x,a,b,c,values,"") in w ++ parse(goToNextLine(goto(NList xs,s)),v,k)
--parse ((NList ((NSymbol "DO"):(NFloat s):(NSymbol x):(NSymbol "="):(NFloat a):(NFloat b):xs)),values,k) = let (v,w) = loop(cut(NList xs,s,[]),x,a,b,1,values,"") in w ++ parse(goToNextLine(goto(NList xs,s)),v,k)
parse ((NList ((NSymbol "DO"):xs)),values,k) = (NList xs,values,k,NSymbol "DO","")
parse ((NList (NSymbol "READ":(NSymbol x):(NEndLine):xs)),values,k) = (NList xs,values,k,NSymbol "READ",x)
parse ((NList (NSymbol "READ":(NSymbol x):xs)),values,k) = (NList xs,values,k,NSymbol "READ2",x)
parse r = error "error"