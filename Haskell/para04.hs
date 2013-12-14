data Map a b = M [(a, b)] 

empty = M []

insert (M list) a b = M ((a,b):list)

createMap l1 l2 = M (zip l1 l2)

find (M list) a = foldl (\acc (s,b) -> if a == s && acc == Nothing then Just b else acc) Nothing list

remove (M list) a = M (rem list) where
  rem [] = []
  rem (x@(s,b):xs) = if s == a then xs else x:(rem xs)
 
fold f (M list) acc = foldl f' acc list where f' c (a,b) = f a b c