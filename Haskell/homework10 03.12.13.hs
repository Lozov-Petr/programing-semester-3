
class Array a where
  lengthA  :: a x -> Int
  getA     :: a x -> Int -> x
  putA     :: a x -> Int -> x -> a x
  subA     :: a x -> Int -> Int -> a x
  mapA     :: (x -> y) -> a x -> a y
  mapiA    :: (Int -> x -> y) -> a x -> a y
  foldA    :: (x -> y -> y) -> a x -> y -> y
  foldiA   :: (Int -> x -> y -> y) -> a x -> y -> y
  concatA  :: a x -> a x -> a x
  zipWithA :: (x -> y -> z) -> a x -> a y -> a z 
  
  zipA     :: a x -> a y -> a (x,y)
  zipA     = zipWithA (,)
  map2A    :: (x -> y -> z) -> a x -> a y -> a z
  map2A f  = ((mapA . uncurry) f .) . zipA

  
  
data A x = A Int (Int -> x)

createA l f = A l $ \s -> if s >= 0 && s < l then f s else error "Error!"

instance Show x => Show (A x) where
  show (A l f) = foldr (\c acc -> acc ++ show (f c) ++ ",") "[|" [l - 2, l - 3..0] ++ (if l > 0 then show (f $ l - 1) else "") ++ "|]"
  
instance Array A where

  lengthA (A l _) = l
  
  getA (A _ f) n = f n
  
  putA (A l f) n x = A l $ \s -> if s == n && s >= 0 && s < l then x else f s
  
  subA (A l f) n m = let l' = min (l - n) m in A l' $ \s -> if s >= 0 && s < m then f $ n + s else error "Error!"
  
  mapA fm (A l f) = A l (fm . f)
  
  mapiA fm (A l f) = A l (\n -> fm n $ f n)
  
  foldA ff (A l f) x = foldr (ff . f) x [l - 1,l - 2..0]
  
  foldiA ff (A l f) x = foldr (\n -> ff n $ f n) x [l - 1,l - 2..0]
  
  concatA (A l1 f1) (A l2 f2) = A (l1 + l2) $ \s -> if s < l1 then f1 s else f2 (s - l1)

  zipWithA f (A l1 f1) (A l2 f2) = let l = min l1 l2 in A l $ \s -> if s < l then f (f1 s) (f2 s) else error "Error!"



