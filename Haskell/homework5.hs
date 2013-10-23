data Tree a = T Integer a (Tree a) (Tree a) | E

instance Show a => Show (Tree a) where
  show tree = printTree "" tree where
    printTree str E = "E"
    printTree str (T h k l r) = node ++ "--" ++ printTree newStr r ++ "\n" ++ str ++ "|\n" ++ str ++ printTree str l where
      node = show (h,k)
      newStr = str ++ "|" ++ map (\_ -> ' ') [1..length node + 1]

insert E x = T 1 x E E
insert t@(T _ k l r) x = balance k newL newR where
  newL = if x < k then insert l x else l
  newR = if k < x then insert r x else r
  balance k l r =
    if hl - hr > 1 then rotateR k (if hll > hlr then l else rotateL kl hll ll lr) hr r else 
    if hr - hl > 1 then rotateL k hl l (if hrr > hrl then r else rotateR kr rl hrr rr) else
    T (1 + max hl hr) k l r where
  
      ht (T h _ _ _) = h
      ht _ = 0
      
      rotateR k (T _ kl ll lr) hr r = T (hr + 2) kl ll (T (hr + 1) k lr r)
      rotateL k hl l (T _ kl rl rr) = T (hl + 2) kr (T (hl + 1) k l rl) rr
      
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