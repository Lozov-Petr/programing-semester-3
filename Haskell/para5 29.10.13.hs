type Parser a = String -> [(a,String)]

sym :: Char -> Parser Char
sym c (t:ts) |c == t = [(c, ts)]
sym _ _ = []

val :: a -> Parser a
val a str = [(a,str)]

infixl 2 |||
(|||) :: Parser a -> Parser a -> Parser a
p1 ||| p2 = \s -> p1 s ++ p2 s

infixl 3 ||>
(||>) :: Parser a -> (a -> Parser b) -> Parser b
p ||> q = \s -> concat [q a s | (a, s) <- p s] 
--(p1 ||> p2) s = foldl (\acc (a,s) -> (p2 a s) ++ acc) [] (p1 s) 

many :: Parser a -> Parser [a]
many a = a ||> (\x -> many a ||> val . (x:)) ||| val []

opt :: Parser a -> Parser (Maybe a)
opt a = a ||> val . Just ||| val Nothing