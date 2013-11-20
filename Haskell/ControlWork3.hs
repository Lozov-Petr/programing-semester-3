-- Задача №1

data Said = L | R deriving (Show, Eq)
type State = (Said,Said,Said,Said) -- 1 - Берег, на котором находится коза;
                                   -- 2 - берег, на котором находится капуста;
                                   -- 3 - берег, на котором находится волк;
                                   -- 4 - берег, везде которого находится лодка.
type Branch = [State]

solutionsProblem :: [Branch]
solutionsProblem = map reverse $ solPr [[(L,L,L,L)]] where
  
  solPr :: [Branch] -> [Branch]
  solPr list = if isEnd curSolution then curSolution else solPr curSolution where
    
    isEnd :: [Branch] -> Bool
    isEnd = foldr (\(x:_) acc -> x == (R,R,R,R) && acc) True
    
    curSolution :: [Branch]
    curSolution = (filter notCycle . filter notEating . concat . map transit) list where
      
      transit :: Branch -> [Branch]
      transit x@(x0:_) = transit' x0 x0 where
        
        transit' :: State -> State -> [Branch]
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
          
      notEating :: Branch -> Bool
      notEating ((a,b,c,d):_)
        | a == b && b /= d = False
        | a == c && c /= d = False
        | True             = True
        
      notCycle :: Branch -> Bool
      notCycle (x:xs) = not $ elem x xs
     
-- Типы данных для задач 2, 3, 4

data Gen                 = A | T | G | C deriving (Show, Eq)
type ThreeGens           = (Gen,Gen,Gen)
type ListThreeGensAndInt = [(ThreeGens,Integer)]
type FunGenToInt         = ThreeGens -> Maybe Integer
type FunIntToGen         = Integer -> Maybe ThreeGens

-- Задача №2

complement :: [Gen] -> [Gen]
complement = map comp where
  
  comp :: Gen -> Gen
  comp x = case x of {A -> T; T -> A; G -> C; C -> G}

-- Задача №3

createFunGenToInt :: ListThreeGensAndInt -> FunGenToInt
createFunGenToInt list = \g1 -> foldr (\(g,i) acc -> if g == g1 then Just i else acc) Nothing list

genToInt :: FunGenToInt -> [Gen] -> [Integer]
genToInt fun (a:l1@(b:c:l2)) =
  case fun (a,b,c) of
    Just s -> s : genToInt fun l2
    _      -> genToInt fun l1
genToInt _ _ = []

--  Задача №4

createFunIntToGen :: ListThreeGensAndInt -> FunIntToGen
createFunIntToGen list i1 = foldr (\(g,i) acc -> if i == i1 then Just g else acc) Nothing list

intToGen :: FunIntToGen -> [Integer] -> [Gen]
intToGen fun (x:xs) =
  case fun x of
    Just (a,b,c) -> a : b : c : intToGen fun xs
    _            -> intToGen fun xs
intToGen _ _ = []
 
-- Тесты

listForFuns = [((A,T,C),2),((C,T,T),5),((A,C,G),0)]

testGen  = [A,T,C,C,T,T,G,G,G,G,G,A,C,G,G,G,G,G,G,G,G,G,A,T,C,G]
------------__2__|__5__|_________|__0__|_______________|__2__|_

testList = [  2,  5,  7,  0,  2]
------------ATC|CTT|___|ACG|ATC

fun1 = createFunGenToInt listForFuns
fun2 = createFunIntToGen listForFuns

res1 = complement testGen
res2 = genToInt fun1 testGen
res3 = intToGen fun2 testList
