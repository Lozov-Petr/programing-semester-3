type Parser a = String -> [(a,String)]

empty :: Parser a
empty _ = []

sym :: Char -> Parser Char
sym c (t:ts) |c == t = [(c, ts)]
sym _ _ = []

val :: a -> Parser a
val a str = [(a,str)]

infixl 2 |||
(|||) :: Parser a -> Parser a -> Parser a
p1 ||| p2 = \s ->  p1 s ++ p2 s

infixl 3 ||>
(||>) :: Parser a -> (a -> Parser b) -> Parser b
p ||> q = \s -> concat [q a s | (a, s) <- p s]

many :: Parser a -> Parser [a]
many par = par ||> (\x -> many par ||> val . (x:)) ||| val []

opt :: Parser a -> Parser (Maybe a)
opt a = a ||> val . Just ||| val Nothing

eof :: [(a, String)] -> [a]
eof = map fst . filter ((==[]) . snd)

--------------------------------------------------------------------------------------------

--  ident : letter (letter|digit)*
--  literal : digit digit*
--  primary : ident|literal|'('expr')'
--  multi : primary ('*'|'/') multi|primary ~~ primory (op primory)*

--  expr : multy

-- "починить" ассоциативность
--  свернуть функции

data E = X String 
       | N Integer 
       | Mul E E -- *
       | Div E E -- / 
       | Add E E -- +
       | Sub E E -- -
       | Gt  E E -- >
       | Lt  E E -- <
       | Eq  E E -- ==
       | Ngt E E -- <=
       | Nlt E E -- >=
       | Neq E E -- !=
       | And E E -- &&
       | Or  E E -- ||
       deriving Show

oneOf = foldl (\a b -> a ||| sym b) empty

letter = oneOf $ '_':(['a'..'z'] ++ ['A'..'Z'])
digit = oneOf ['0'..'9']

ident = letter ||> (\x -> many (letter ||| digit) ||> (\xs -> val $ X (x:xs)))

literal = digit ||> (\x -> many digit ||> (\xs -> val $ N $ read (x:xs)))

primary = ident 
       ||| literal 
       ||| sym '(' ||> (\ _ -> expr ||> (\ e -> sym ')' ||> (\ _ -> val e)))

multi = primary ||> (\x -> op ||> (\o -> multi ||> (\y -> val $ x `o` y))) ||| primary
  where op = sym '*' ||> (\_ -> val Mul) ||| 
             sym '/' ||> (\_ -> val Div)

addi = multi ||> (\x -> op ||> (\o -> addi ||> (\y -> val $ x `o` y))) ||| multi
  where op = sym '+' ||> (\_ -> val Add) ||| 
             sym '-' ||> (\_ -> val Sub)

reli = addi ||> (\x -> op ||> (\o -> addi ||> (\y -> val $ x `o` y))) ||| addi
  where op = sym '>' ||> (\_ -> val  Gt) |||
             sym '<' ||> (\_ -> val  Lt) |||
             sym '=' ||> (\_ -> sym '=') ||> (\_ -> val  Eq) |||
             sym '!' ||> (\_ -> sym '=') ||> (\_ -> val Neq) |||
             sym '>' ||> (\_ -> sym '=') ||> (\_ -> val Nlt) |||
             sym '<' ||> (\_ -> sym '=') ||> (\_ -> val Ngt)
  
logi = reli ||> (\x -> op ||> (\o -> logi ||> (\y -> val $ x `o` y))) ||| reli
  where op = sym '&' ||> (\_ -> sym '&') ||> (\_ -> val And) |||
             sym '|' ||> (\_ -> sym '|') ||> (\_ -> val  Or)

expr = logi 