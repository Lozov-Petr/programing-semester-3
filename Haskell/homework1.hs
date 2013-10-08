gcd' 0 0 = error "Не определено"
gcd' a b
  | b == 0 = abs a
  | True = gcd' b (rem a b)

lcm' a b = div (a * b) (gcd' a b)


is_coprime a b = gcd' a b == 1


euler_func a = if a >= 0 then euler_func' a 0 else error "Не определено" where
  euler_func' b c
    | b == 1 = c + 1
    | is_coprime a b = euler_func' (b - 1) (c + 1)
    | True = euler_func' (b - 1) c


euler_func2 a = length (filter (is_coprime a) [1 .. (a - 1)])