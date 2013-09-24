concat_list = foldl (++) []

listify = map (:[])

listify' = foldr (\y x -> [y] : x) []

listify'' = foldl (\x y -> x ++ [[y]]) []
l = 1 : l
n = 1 : [x+1 | x <- n] 

