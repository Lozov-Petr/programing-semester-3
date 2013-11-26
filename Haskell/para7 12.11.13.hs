type Parser a = String -> [(a,String)]

empty :: Parser a
empty = \s -> []

sym :: Char -> Parser Char
sym c (t:ts) |c == t = [(c, ts)]
sym _ _              = []

val :: a -> Parser a
val a str = [(a,str)]

infixl 2 |||
(|||) :: Parser a -> Parser a -> Parser a
p1 ||| p2 = \s -> p1 s ++ p2 s

infixl 3 ||>
(||>) :: Parser a -> (a -> Parser b) -> Parser b
p ||> q = \s -> concat [q a s | (a,s) <- p s] 

many :: Parser a -> Parser [a]
many par = par ||> (\a -> many par ||> val . (a:)) ||| val []

opt :: Parser a -> Parser (Maybe a)
opt a = a ||> val . Just ||| val Nothing

eof :: [(a, String)] -> [a]
eof = map fst . filter ((==[]) . snd)

------------------------------------------------------------------------------------------

data E = Var String   -- Переменная
       | Num Integer  -- Число
       | Mul E E      --  *
       | Div E E      --  /
       | Add E E      --  +
       | Sub E E      --  -
       | Les E E      --  <
       | LoE E E      -- <=
       | Equ E E      -- ==
       | NEq E E      -- !=	     
       | MoE E E      -- >=
       | Mor E E      --  >
       | Or  E E      -- ||
       | And E E      -- &&

data Assoc = LeftAssoc | RightAssoc | NotAssoc      
       
oneOf = foldr ((|||) . sym) empty

letter = oneOf $ '_':(['a'..'z'] ++ ['A'..'Z'])
digit  = oneOf ['0'..'9']

literal = digit  ||> (\a -> many digit ||> \b -> val $ Num $ read (a:b))
ident   = letter ||> (\a -> many (letter ||| digit) ||> \b -> val $ Var (a:b))

primary = ident ||| literal
  	              ||| sym '(' ||> (\_ -> expr ||> (\a -> sym ')' ||> \_ -> val a))
	  
multi   = allOptExpr primary      multi   ["*","/"]                     [Mul,Div]	               LeftAssoc 
addi    = allOptExpr (multi id)   addi    ["+","-"]                     [Add,Sub]                 LeftAssoc 
reli    = allOptExpr (addi id)    addi    ["<","<=","==","!=",">=",">"] [Les,LoE,Equ,NEq,MoE,Mor] NotAssoc 
logiAnd = allOptExpr (reli id)    logiAnd ["&&"]                        [And]                     RightAssoc
logiOr  = allOptExpr (logiAnd id) logiOr  ["||"]                        [Or]                      RightAssoc
 
allOptExpr :: Parser E -> ((E -> E) -> Parser E) -> [String] -> [E -> E -> E] -> Assoc -> (E -> E) -> Parser E
allOptExpr par1 par2 lStr lOp assoc hole = par1 ||> (\a -> op ||> (\o -> val (newHole assoc hole a o) ||> par2)) ||| par1 ||> (val . hole)  where 
  op = foldl (\acc (s, f) -> acc ||| prefix s ||> \_ -> val f) empty $ zip lStr lOp where
	  prefix [x]    = sym x
	  prefix (x:xs) = sym x ||> \_ -> prefix xs
  
  newHole LeftAssoc  hole a o = o $ hole a                     
  newHole _          hole a o = hole . (o a)
 
expr = logiOr id

------------------------------------------------------------------------------------------

instance Show E where
  show tree = "\n    " ++ printTree "    " tree ++ "\n" where
    printTree _ (Num a) = show a
    printTree _ (Var s) = s
    printTree str x = case x of
              Mul l r -> print "*"  l r
              Div l r -> print "/"  l r
              Add l r -> print "+"  l r
              Sub l r -> print "-"  l r
              Les l r -> print "<"  l r
              LoE l r -> print "<=" l r
              Equ l r -> print "==" l r
              NEq l r -> print "!=" l r
              MoE l r -> print ">=" l r
              Mor l r -> print ">"  l r
              Or  l r -> print "||" l r
              And l r -> print "&&" l r
      where
        print s1 l r = "[" ++ s1 ++ "]--" ++ printTree newStr r ++ "\n" ++ str ++ "|\n" ++ str ++ printTree str l where
          newStr = str ++ "|" ++ map (\_ -> ' ') [1..length s1 + 3]