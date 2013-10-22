data Tree = T Integer (Integer, Tree) (Integer, Tree) | E

instance Show Tree where
  show E = "E"
  show (T k l r) = (show k) ++ " " ++(show l) ++ " " ++(show r)
  
insert (0, E) x = (1, T x (0, E) (0, E))
insert (_, T k l r) x = balance $ if x < k then T k (insert l x) r else T k l (insert r x) where
  balance t@(T k l r) =
    if hl - hr > 1 then
      if hll - hlr > 0 then (hll + 1, T kl ll (hr + 1, T k lr r))
      else (hlr + 1, T klr (hll + 1, T kl ll lrl) (hr + 1, T k lrr r))
    else if hr - hl > 1 then
      if hrr - hrl > 0 then (hrr + 1, T kr (hl + 1, T k l rl) rr)
      else (hrl + 1, T krl (hl + 1, T k l rll) (hrr + 1, T kr rlr rr))
    else (1 + max hl hr, t) where
      (hl, tl) = l
      (T kl ll@(hll, _) lr) = tl
      (hr, tr) = r
      (T kr rl rr@(hrr, _)) = tr
      (hlr, tlr) = lr
      (T klr lrl lrr) = tlr
      (hrl, trl) = rl
      (T krl rll rlr) = trl



insertList tree list = foldl insert tree list
createTree = insertList (0, E)