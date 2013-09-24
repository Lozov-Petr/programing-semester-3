  -- Задание №01 --

zipWith' f (x:xs) (y:ys) = (f x y) : (zipWith' f xs ys)
zipWith' f l1 l2 = []

  -- Задание №02 --

sprod x y  = foldl (+) 0 (zipWith' (*) x y) 

  -- Задание №03 --

normalize (x:xs) = x : (normalize $ filter (/=x) xs)
normalize [] = []

  -- Задание №04 --

sort (x:xs) = (sort $ filter (<=x) xs) ++ [x] ++ (sort $ filter (>x) xs)
sort [] = []

  -- Задание №05 --

isProgression (x1:xs@(x2:_)) = isProg xs where
  isProg (y1:ys@(y2:_)) = if y2 - y1 == diff then isProg ys else False where diff = x2 - x1
  isProg y = True   
isProgression x = True

  -- Задание №06 --  

isFunction ((x,_):xs) = (foldl (\acc (s, _) -> acc && (s /= x)) True xs) && isFunction xs
isFunction [] = True

  -- Задание №07 --  

isSymmetric ((x,y):xs) 
  | x == y = isSymmetric xs 
  | snd res = isSymmetric $ fst res 
  | True = False 
    where res = foldr (\s (acc,isSym) -> if s == (y,x) then (acc, True) else (s:acc,isSym)) ([], False) xs
isSymmetric [] = True

  -- Задание №08 -- 

isReflexive r = foldr fun True [1..] where 
  fun a acc = (elem (s,s) r) && acc where
    s = ((rem a 2) * 2 - 1) * (div a 2)  
  
isReflexive' r = isRef (normalize $ foldl (\acc (a,b) -> a:b:acc) [] r) where
  isRef (h:hs) = (foldl (\acc (a, b) -> if (h == a) && (a == b) then True else acc) False r) && isRef hs
  isRef [] = True
  
  -- Задание №09 -- 

closure r = r ++ [(a,d) | (a,b) <- r, (c,d) <- r, b == c, not $ elem (a,d) r]

  -- Задание №10 -- 
  
isTransitive x = x == (closure x)