fib n = fib' (0, 1) 1 where 
    fib' (a, b) i 
     | i == n = b
     | True = fib' (b, a + b) (i + 1)
--------------------------------------------------------------------------------------------------------------
reduce (a, b) 
 | b == 0 = error "Знаменатель равен нулю"
 | a == 0 = (0, 1)
 | True = (div a (gcd a b), div b (gcd a b)) 

infixl 6 +/
(a, b) +/ (c, d) = if b == 0 || d == 0 then error "Знаменатель равен нулю" else reduce (a * d + b * c, b * d)

infixl 6 -/
(a, b) -/ (c, d) = (a, b) +/ ((-c), d)

infixl 7 */
(a, b) */ (c, d) = if b == 0 || d == 0 then error "Знаменатель равен нулю" else reduce (a * c, b * d)

infixl 7 //
(a, b) // (c, d) =  (a, b) */ (d, c) 
--------------------------------------------------------------------------------------------------------------

len [] = 0
len (hd : tl) = 1 + len tl

con [] x = x
con (hd : tl) x = hd : (con tl  x)

rev x = rev' x [] where
 rev' [] x = x
 rev' (hd : tl) x = rev' tl (hd : x)

take' _ [] = []
take' 0 _ = []
take' n (hd : tl) = hd : take' (n - 1) tl
---------------------------------------------------------------------------------------------------------------

concat' [] = []
concat' (hd : tl) = hd ++ concat tl

l1 *** l2 = dec l1 l2 where
 dec [] (hd : tl) = dec l1 tl
 dec _ [] = []
 dec (hd1 : tl1) (hd2 : tl2) = (hd1, hd2) : (dec tl1 (hd2 : tl2))
