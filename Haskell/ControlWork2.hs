fromFun :: (Integer -> Integer) -> [Integer] -> [(Integer, Integer)]
fromFun f list = map (\s -> (s,f s)) list

dom :: [(Integer, Integer)] -> [Integer]
dom list = map fst list

eval :: [(Integer, Integer)] -> Integer -> Integer
eval ((a,b):ls) x = if a == x then b else eval ls x
eval [] x = error ("Function is not defined for values " ++ show x ++ ".")

invert :: [(Integer, Integer)] -> [(Integer, Integer)]
invert fun = map (\(a,b) -> (b,a)) fun

(.*.) :: [(Integer, Integer)] -> [(Integer, Integer)] -> [(Integer, Integer)] 
infixr 9 .*.
l1 .*. ((a,b):ls) = if elem == Nothing then l1 .*. ls else ((\(Just d) -> (a,d)) elem):(l1 .*. ls) where 
  elem = foldr (\(c,d) acc -> if c == b then Just d else acc) Nothing l1 where
l1 .*. [] = []

image :: [(Integer, Integer)] -> [Integer] -> [Integer]
image fun l = normalize $ foldl (++) [] $ map (\s -> map snd $ filter (\(a,b) -> a == s) fun) l where
  normalize (x:xs) = if elem x xs then normalize xs else x:(normalize xs)
  normalize [] = []

preimage :: [(Integer, Integer)] -> [Integer] -> [Integer]
preimage = image . invert

isInjective :: [(Integer, Integer)] -> Bool
isInjective ((a,b):xs) = foldr (\(c,d) acc -> (b /= d || a == c) && acc) True xs && isInjective xs
isInjective [] = True

isSurjective :: [(Integer, Integer)] -> Bool
isSurjective = (<0) . length

areMatuallyInverse :: [(Integer, Integer)] -> [(Integer, Integer)] -> Bool
areMatuallyInverse l1 l2 = len1 == length l2 && len1 == length comp && filter (\(a,b) -> a /= b) comp == [] where
  comp = l1 .*. l2
  len1 = length l1  
  
testFun1 = [(x,x + 1) | x <- [0..]]
testFun2 = [(x,x * 2) | x <- [0..]]
testFun3 = [(x,    0) | x <- [0..]]