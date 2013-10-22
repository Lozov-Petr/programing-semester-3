data Tree = T Integer Integer Tree Tree | E

instance Show Tree where
  show E = "E"
  show (T h k l r) = "(|" ++ (show h) ++ "|" ++ (show k) ++ " " ++(show l) ++ " " ++(show r) ++ ")"

  
insert E x = T 1 x E E
insert (T _ k l r) x = balance k newL newR  if x < k then T k (insert l x) r else T k l (insert r x) where
  newL = if x < k then insert l x else l
  newR = if x < k then r else insert r x
  balance k l r =
    if hl - hr > 1 then
      if hll - hlr > 0 then T (hll + 1) kl ll (T (hr + 1) k lr r)
      else T (hlr + 1) klr (T (hll + 1) kl ll lrl) (T (hr + 1) k lrr r))
    else if hr - hl > 1 then
      if hrr - hrl > 0 then (T (hrr + 1) kr (T (hl + 1) k l rl) rr)
      else (klr + 1, T krl (T (hl + 1) k l rll) (T (hrr + 1) kr rlr rr))
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