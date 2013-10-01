--Алгебраические типы данных

--Конструктор типов

-- a -> b, a,b - типы

-- a -> (b -> (c -> ...))

-- () - unit

-- (a,b); (a,b,c) - конструкторы кортежей

data Color = Red|Green|Blue
-- имя типа   конструкторы

colors = [Red, Green, Blue]

colorsString x = map (\s -> case s of
                              Red   -> "Red"
                              Green -> "Green"
                              Blue  -> "Blue"
                     ) x                               
                     
data Sound = Sound Int Int

sounds = map (Sound 1) [3,4,5,6] 
inRange (Sound _ j) = j >= 20 && j <= 2000 

data L a = Cons a (L a) | Nil
-- data List a = a ':' (List a) | []

len Nil = 0
len (Cons _ l) = len l + 1

data Tree a = E | T (Tree a) a (Tree a)

nnodes E = 0
nnodes (T l _ r) = nnodes l + nnodes r + 1

nleavs E = 0
nleavs (T E _ E) = 1
nleavs (T l _ r) = nleavs l + nleavs r

depth E = 0
depth (T l _ r) = 1 + max (depth l) (depth r)

leaves = leaves' [] where
  leaves' acc E = acc
  leaves' acc (T E x E) = x:acc
  leaves' acc (T l x r) = leaves' (leaves' acc l) r

maxn E = error "!!!"
maxn (T l x r) = maxn' (maxn' x l) r where
  maxn' x E = x
  maxn' x (T l y r) = maxn' (maxn' (max x y) l) r  
  

factor n E = (0,0)
factor n (T l x r) = (factor n l !+ factor n r) !+ if x < n then (1,0) else (0,1) where
  (a,b) !+ (c,d) = (a + c,b + d)

--ДЗ

--insert :: Tree Integer -> Integer -> Tree Integer   
--добавляет в двоичное дерево число

--find :: Tree Integer -> Integer -> Maybe (Tree Integer)
--Находит поддерево с заданным корнем

--isTree :: Tree Integer -> Bool
--Проверяет, является ли дерево деревом поиска

--elements :: Tree Integer -> [Integer]
