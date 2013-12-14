class (Ord a, Show (m a b)) => Map m a b where
  empty     :: m a b
  insert    :: m a b -> a -> b -> m a b
  find      :: m a b -> a -> Maybe b
  remove    :: m a b -> a -> m a b
  fold      :: (a -> b -> s -> s) -> m a b -> s -> s
  createMap :: [a] -> [b] -> m a b
  createMap l1 l2 = foldl (\acc (a,b) -> insert acc a b) empty $ zip l1 l2 
  
data M a b = T Integer a [b] (M a b) (M a b) | E

instance (Show a, Show b) => Show (M a b) where
  show tree = printTree "" tree where
    printTree _ E = "E"
    printTree str (T h k i l r) = 
      node ++ "--" ++ printTree newStr r ++ "\n" ++ str ++ "|\n" ++ str ++ printTree str l where
        node = show (h,k,i)
        newStr = str ++ "|" ++ map (\_ -> ' ') [1..length node + 1]
        

--------- Balance ---------
balance k i l r =
  if hl - hr > 1 then rotateR k i (if hll > hlr then l else rotateL kl il hll ll lr) hr r else 
  if hr - hl > 1 then rotateL k i hl l (if hrr > hrl then r else rotateR kr ir rl hrr rr) else
  T (1 + max hl hr) k i l r where
    
    ht (T h _ _ _ _) = h
    ht _ = 0
    
    rotateR k i (T _ kl il ll lr) hr r = T (hr + 2) kl il ll (T (hr + 1) k i lr r)
    rotateL k i hl l (T _ kr ir rl rr) = T (hl + 2) kr ir (T (hl + 1) k i l rl) rr
    
    T _ kl il ll lr = l
    T _ kr ir rl rr = r      
    hl = ht l
    hr = ht r
    hll = ht ll
    hlr = ht lr
    hrl = ht rl
    hrr = ht rr

    
-------- Instance ---------
instance (Ord a, Show (M a b)) => Map M a b where
  ---------- Empty ----------
  empty = E

  --------- Insert ----------
  insert E x y = T 1 x [y] E E
  insert (T h k i l r) x y  
    | x < k = balance k i (insert l x y) r
    | x > k = balance k i l (insert r x y)
    | True  = T h k (y:i) l r

  ---------- Find ----------- 
  find E _ = Nothing
  find (T _ k (i:_) l r) x 
    | x < k = find l x
    | x > k = find r x
    | True  = Just i

  --------- Remove ----------  
  remove E _ = E
  remove (T h k i@(_:is) l r) x
    | x < k         = balance k i (remove l x) r
    | x > k         = balance k i l (remove r x)
    | not $ null is = T h k is l r
    | h == 1        = E
    | isEmpty r     = l
    | isEmpty l     = r
    | True     = balance newK newI l newR 
    where
      
      (newK, newI, newR) = removeMin r where
        removeMin (T _ k i l r)
          | isEmpty l = (k, i, r)
          | True      = (k', i', balance k i l' r) where
            (k', i', l') = removeMin l
            
      isEmpty E = True
      isEmpty _ = False
   
  ---------- Fold ----------- 
  fold f (T _ k (i:_) l r) acc = fold f l $ f k i $ fold f r acc 
  fold _ _ acc = acc