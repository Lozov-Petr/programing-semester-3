infixl 8 +/
infixl 8 -/
infixl 9 */
infixl 9 //

infixl 8 |+|
infixl 8 |-|
infixl 9 |*|
infixl 9 |/|
infixl 8 |%|

reduce (a, 0) = error "Знаменатель равен нулю"
reduce (0, b) = (0, 1)
reduce (a, b) = if b > 0 then(div a g, div b g) else reduce (-a, -b) where g = gcd a b 

(a, b) +/ (c, d) = if b == 0 || d == 0 then error "Знаменатель равен нулю" else reduce (a * d + b * c, b * d)

(a, b) -/ (c, d) = (a, b) +/ ((-c), d)

(a, b) */ (c, d) = if b == 0 || d == 0 then error "Знаменатель равен нулю" else reduce (a * c, b * d)

(a, b) // (c, d) =  (a, b) */ (d, c) 


x |+| y = (reverse . del_zeros . reverse) (add x y) where 
  add (x : xs) (y : ys) = ((x +/ y) : (add xs ys))
  add x y = x ++ y
  del_zeros ((0, 1) : xs) = del_zeros xs
  del_zeros [] = [(0,1)]
  del_zeros x = x
	
x |*| y = if x == [(0, 1)] || y == [(0, 1)] then [(0, 1)] else mul x 0 where
  mul (x : xs) i = (mul xs (i + 1)) |+| ([(0, 1) | a <- [1 .. i]] ++ (map (*/ x) y))
  mul [] _ = [(0, 1)] 

x |-| y = x |+| ([((-1), 1)] |*| y)

x |/| y = 
  if y == [(0, 1)] then error "Деление на ноль" else if length y > length x || x == [(0, 1)] then [(0, 1)] else div' x (length x) where
  div' x i = if len_y <= i then (div' (x |-| ((++) ([(0, 1) | a <- [1 .. i - len_y]]) (quotient |*| y))) (i - 1)) ++ quotient else [] where
    len_y = length y
    quotient = if length x /= i then [(0, 1)] else [(head (reverse x)) // (head (reverse y))]

x |%| y = x |-| x |/| y |*| y 	

polyGCD x y = if y /= [(0, 1)] then polyGCD y (x |%| y) else x
