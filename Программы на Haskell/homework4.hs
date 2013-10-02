data Tree = T Tree Integer Tree | E

insert (T l u r) x = if x < u then T (insert l x) u r else T l u (insert r x)
insert E x = T E x E

find tree@(T l u r) x
  | x < u  = find l x
  | x > u  = find r x
  | x == u = Just tree
find E x = Nothing

elements (T l u r) = (elements l) ++ [u] ++ (elements r)
elements E = []

isTree E = True
isTree tree = a where 
  (a,_,_) = isTree' tree where
    isTree' (T E u E) = (True,u,u)
    isTree' (T l u E) = (maxL < u && b,minL,u) where (b,minL,maxL) = isTree' l
    isTree' (T E u r) = (u <= minR && b,u,maxR) where (b,minR,maxR) = isTree' r
    isTree' (T l u r) = (u <= minR && maxL < u && b1 && b2,minL,maxR) where 
      (b1,minL,maxL) = isTree' l
      (b2,minR,maxR) = isTree' r

isTree1 tree = foldr (\x acc -> x >= 0 && acc) True $ zipWith (-) xs x where x@(_:xs) = elements tree 
--isTree1 - плохая функция, так как T((T E 1 E) 1 (T E 1 E)) -> True

insertList tree (x:xs) = insertList (insert tree x) xs
insertList tree [] = tree

createTree x = insertList E x

treeToString E = "E"
treeToString (T l u r) = "T(" ++ (treeToString l) ++ " " ++ (show u) ++ " " ++ (treeToString r) ++ ")" where

maybeTreeToString Nothing = "Nothing"
maybeTreeToString (Just x) = "Just " ++ (treeToString x) 

tree = createTree [8,4,12,2,6,10,14,1,3,5,7,9,11,13,15]
tree1 = T (T (T E 4 E) 4 E) 5 (T E 6 (T E 6 E))