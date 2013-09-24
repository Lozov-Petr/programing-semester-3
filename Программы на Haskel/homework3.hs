fib = map (fst) l where l = ((0, 1) : [(b, a + b) | (a, b) <- l])

primes = 2 : [x | x <- [3,5..], is_prime x] where 
  is_prime x = foldr (\l acc -> l * l > x || (acc && rem x l /= 0)) True primes
