data E = X String | C Int | A E E | S E E | M E E | D E E

data Value a = Error String | Value a deriving Show

instance Monad Value where
  return x = Value x
  Value a >>= f = f a
  Error s >>= _ = Error s
  
eval s (X n  ) = s n
eval s (C a  ) = return a
eval s (A a b) = do x <- eval s a
                    y <- eval s b
                    return $ x + y
eval s (S a b) = do x <- eval s a
                    y <- eval s b
                    return $ x - y
eval s (M a b) = do x <- eval s a
                    y <- eval s b
                    return $ x * y
eval s (A a b) = do x <- eval s a
                    y <- eval s b
                    if y == 0 
                      then Error "Division by zero." 
                      else Value $ div x y

test = S (A (C 1) (C 2)) (S (C 9) (C 7))
