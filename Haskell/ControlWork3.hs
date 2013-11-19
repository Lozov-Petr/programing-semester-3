-- Задача №1

type State = (Bool,Bool,Bool,Bool) -- 1 - Коза на левом берегу?
                                   -- 2 - Капуста на левом берегу?
                                   -- 3 - Волк на левом берегу?
                                   -- 4 - Лодочник на левом берегу?

solutionsProblem = solPr [[(True,True,True,True)]] where
  solPr list = if isEnd curSolution then curSolution else solPr curSolution where
    
    isEnd = foldr (\(x:_) acc -> x == (False,False,False,False) && acc) True
    
    curSolution = (filter notCycle . filter notEating . concat . map transit) list where
      
      transit x@(x0:_) = transit' x0 x0 where
        transit' (a,b,c,_) s =
          case s of
          (False,False,False,False) -> [x]
          (True ,_    ,_    ,True ) -> ((False,b,c,False):x) : transit' x0 (False,b    ,c    ,True)
          (False,True ,_    ,True ) -> ((a,False,c,False):x) : transit' x0 (False,False,c    ,True)
          (False,False,True ,True ) -> ((a,b,False,False):x) : transit' x0 (False,False,False,True)
          (False,False,False,True ) -> [(a,b,c,False): x]
          (False,_    ,_    ,False) -> ((True,b,c,True):x) : transit' x0 (True,b   ,c   ,False)
          (True ,False,_    ,False) -> ((a,True,c,True):x) : transit' x0 (True,True,c   ,False)
          (True ,True ,False,False) -> ((a,b,True,True):x) : transit' x0 (True,True,True,False)
          (True ,True ,True ,False) -> [(a,b,c,True) : x]
          
      notEating ((a,b,c,d):_)
        | a == b && b /= d = False
        | a == c && c /= d = False
        | True             = True
        
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