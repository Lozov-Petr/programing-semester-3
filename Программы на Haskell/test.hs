sqr q = sqr' q where 
  sqr' y = if y * y / q < e then y 
                             else sqr' ((y + q / y) / 2) where 
                               e = 1.0001