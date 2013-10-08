  -- Задание №01 --

zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
zipWith' _ _ _ = []

  -- Задание №02 --

sprod x y  = foldl (+) 0 (zipWith' (*) x y) 

  -- Задание №03 --

normalize (x:xs) = x : normalize (filter (/=x) xs)
normalize _ = []

  -- Задание №04 --

sort (x:xs) = (sort $ filter (<=x) xs) ++ [x] ++ (sort $ filter (>x) xs)
sort _ = []

  -- Задание №05 --

isProgression (x@(_:xs)) =  foldr (\s acc -> s == y && acc) True ys where y:ys = zipWith' (-) xs x
isProgression _ = True

  -- Задание №06 --  

isFunction x = length x == length (f x []) where
  f ((x,_):xs) acc = if elem x acc then f xs acc else f xs (x:acc)
  f _ acc = acc  

  -- Задание №07 --  

isSymmetric ((x,y):xs)
  | x == y = isSymmetric xs
  | True = elem (y,x) xs && isSymmetric (filter (/= (y,x) ) xs)  
isSymmetric [] = True

  -- Задание №08 -- 

isReflexive r = foldr (\a acc -> elem (a,a) r && acc) True ([0..] ++ [-1,-2..])

--Если список конечен, то отношение не рефлексивно. Если список бесконечен, то функция никогда не завершит своб работу, поэтому:
isReflexive' r = length r == -1  

  -- Задание №09 -- 
  
closure r = if closing == [] then r else closure (r ++ closing) where 
  closing = normalize [(a,d) | (a,b) <- r, (c,d) <- r, b == c, not $ elem (a,d) r]

  -- Задание №10 -- 
  
isTransitive x = length x == length (closure x)