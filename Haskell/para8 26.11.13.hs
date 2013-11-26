-- Классы типов (для поддержания ad-hoc полиморфизм)

--class Eq a where
--  (==) :: a -> a -> Bool
  
--f :: Eq a => a -> a -> Bool
     --Контекст

--f x y = x == y

-----------------------------------------------------------

--data T = T

--instance Eq T where
--  _ == _ = True
--  _ /= _ = False

--f a@T b@T = a == b

-----------------------------------------------------------

--data T a = T a

--instance (Eq a, Num a) => Eq (T a) where
--  (T x) == (T y) = x == (x + y)
  

-----------------------------------------------------------

--class Eq a => Ord a where
--  (<=) :: a -> a -> Bool

class SemyGroup a where
  (+++) :: a -> a -> a
  
class SemyGroup a => Monoid a where  
  e :: a
  
class Monoid a => Group a where
  inv :: a -> a
  
  
infixl 6 +++
    
instance SemyGroup Int where  
  (+++) = (+)

instance Monoid Int where   
  e = 0

instance Group Int where
  inv a = -a
  
infixl 7 ***  

class Group a => Ring a where
  (***) :: a -> a -> a
  
class Ring a => RingE a where
  eM :: a
  
class RingE a => Field a where
  invM :: a -> a
  
  
-- Описать класс типов для Map

class Ord a => Map m a b where
  empty  :: m a b
  insert :: m a b -> a -> b -> m a b
  find   :: m a b -> a -> Maybe b
  remove :: m a b -> a -> m a b
  fold   :: (a -> b -> s -> s) -> s -> m a b -> s
