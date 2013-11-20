-- Задача №1

data Said = L | R deriving (Show, Eq)
type State = (Said,Said,Said,Said) -- 1 - Берег, на котором коза;
                                   -- 2 - берег, на котором капуста;
                                   -- 3 - берег, на котором волк;
                                   -- 4 - берег, на котором лодка.

solutionsProblem :: [[State]]
solutionsProblem = solPr [[(L,L,L,L)]] where
  
  solPr :: [[State]] -> [[State]]
  solPr list = if isEnd curSolution then curSolution else solPr curSolution where
    
    isEnd :: [[State]] -> Bool
    isEnd = foldr (\(x:_) acc -> x == (R,R,R,R) && acc) True
    
    curSolution :: [[State]]
    curSolution = (filter notCycle . filter notEating . concat . map transit) list where
      
      transit :: [State] -> [[State]]
      transit x@(x0:_) = transit' x0 x0 where
        
        transit' :: State -> State -> [[State]]
        transit' (a,b,c,_) s =
          case s of
          (R,R,R,R) -> [x]
          (L,_,_,L) -> ((R,b,c,R):x) : transit' x0 (R,b,c,L)
          (R,L,_,L) -> ((a,R,c,R):x) : transit' x0 (R,R,c,L)
          (R,R,L,L) -> ((a,b,R,R):x) : transit' x0 (R,R,R,L)
          (R,R,R,L) -> [(a,b,c,R):x]
          (R,_,_,R) -> ((L,b,c,L):x) : transit' x0 (L,b,c,R)
          (L,R,_,R) -> ((a,L,c,L):x) : transit' x0 (L,L,c,R)
          (L,L,R,R) -> ((a,b,L,L):x) : transit' x0 (L,L,L,R)
          (L,L,L,R) -> [(a,b,c,L):x]
          
      notEating :: [State] -> Bool
      notEating ((a,b,c,d):_)
        | a == b && b /= d = False
        | a == c && c /= d = False
        | True             = True
        
      notCycle :: [State] -> Bool
      notCycle (x:xs) = not $ elem x xs
     
-- Типы данных для задач 2, 3, 4

data Gen           = A | T | G | C deriving (Show, Eq)
type ThreeGens     = (Gen,Gen,Gen)
type ListIntAndGen = [(ThreeGens,Integer)]
type FunGenToList  = ThreeGens -> Maybe Integer
type FunListToGen  = Integer -> Maybe ThreeGens

-- Задача №2

complement = map comp where
  comp x = case x of {A -> T; T -> A; G -> C; C -> G}

-- Задача №3

createFunGenToList :: ListIntAndGen -> FunGenToList
createFunGenToList list = \g1 -> foldr (\(g,i) acc -> if g == g1 then Just i else acc) Nothing list

genToList :: FunGenToList -> [Gen] -> [Integer]
genToList fun (a:l1@(b:c:l2)) =
  case fun (a,b,c) of
    Just s -> s : genToList fun l2
    _      -> genToList fun l1
genToList _ _ = []

--  Задача №4

createFunListToGen :: ListIntAndGen -> FunListToGen
createFunListToGen list i1 = foldr (\(g,i) acc -> if i == i1 then Just g else acc) Nothing list

listToGen :: FunListToGen -> [Integer] -> [Gen]
listToGen fun (x:xs) =
  case fun x of
    Just (a,b,c) -> a : b : c : listToGen fun xs
    _            -> listToGen fun xs
listToGen _ _ = []
 
-- Тесты

listForFun = [((A,T,C),2),((C,T,T),5),((A,C,G),0)]

testGen  = [A,T,C,C,T,T,G,G,G,G,G,A,C,G,G,G,G,G,G,G,G,G,A,T,C,G]
------------__2__|__5__|_________|__0__|_______________|__2__|_

testList = [  2,  5,  7,  0,  2]
------------ATC|CTT|___|ACG|ATC

fun1 = createFunGenToList listForFun
fun2 = createFunListToGen listForFun

res1 = complement testGen
res2 = genToList fun1 testGen
res3 = listToGen fun2 testList
