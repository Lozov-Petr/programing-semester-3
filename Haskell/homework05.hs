{--------------------------------
           Лозов Пётр
           Группа 271
            23.10.13
       Вставка в AVL-tree
--------------------------------}

data Tree a = T Integer a (Tree a) (Tree a) | E

------------- Show --------------

instance Show a => Show (Tree a) where
  show tree = printTree "" tree where
    printTree _ E = "E"
    printTree str (T h k l r) = 
      node ++ "--" ++ printTree newStr r ++ "\n" ++ str ++ "|\n" ++ str ++ printTree str l where
        node = show (h,k)
        newStr = str ++ "|" ++ map (\_ -> ' ') [1..length node + 1]
      
--------- Height Tree ----------
 
ht (T h _ _ _) = h
ht _ = 0      

------------ Insert -------------
 
insert E x = T 1 x E E
insert (T _ k l r) x = balance k newL newR where
  newL = if x < k then insert l x else l
  newR = if k < x then insert r x else r
  balance k l r =
    if hl - hr > 1 then rotateR k (if hll > hlr then l else rotateL kl hll ll lr) hr r else 
    if hr - hl > 1 then rotateL k hl l (if hrr > hrl then r else rotateR kr rl hrr rr) else
    T (1 + max hl hr) k l r where
      
      rotateR k (T _ kl ll lr) hr r = T (hr + 2) kl ll (T (hr + 1) k lr r)
      rotateL k hl l (T _ kr rl rr) = T (hl + 2) kr (T (hl + 1) k l rl) rr
      
      T _ kl ll lr = l
      T _ kr rl rr = r      
      hl = ht l
      hr = ht r
      hll = ht ll
      hlr = ht lr
      hrl = ht rl
      hrr = ht rr

      
insertList tree = foldl insert tree
createTree list = insertList E list

------------- Tests -------------

isAVLTree E = True
isAVLTree tree = isSearchTree tree && isAVL tree where

  isSearchTree tree = foldr (\x acc -> x >= 0 && acc) True $ zipWith (-) xs x where 
    x@(_:xs) = elements tree where 
      elements tree = el tree [] where
        el (T _ k l r) acc = el l (k:(el r acc))
        el E acc = acc  
      
  isAVL E = True
  isAVL (T h _ l r) = h == 1 + max hl hr && abs (hl - hr) < 2 && isAVL l && isAVL r where
    hl = ht l
    hr = ht r