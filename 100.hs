type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show,Eq)

up (S (a,b) x y z) = if a==0 then Null else S (a-1,b) x "up" (S (a,b) x y z)

down (S (a,b) x y z) = if a==3 then Null else S (a+1,b) x "down" (S (a,b) x y z)

left (S (a,b) x y z) = if b==0 then Null else S (a,b-1) x "left" (S (a,b) x y z)

right (S (a,b) x y z) = if b==3 then Null else S (a,b+1) x "right" (S (a,b) x y z)

removeItem _ [] = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys
collect (S a x y z) = if elem a x then S a (removeItem a x) "collect" (S a x y z) else Null

nextMyStates (S a x y z) = removeItem Null [up (S a x y z),down (S a x y z),left (S a x y z),right (S a x y z),collect (S a x y z)]

isGoal (S a x y z) = if x==[] then True else False

search ((S a x y z):l) = if isGoal (S a x y z) then S a x y z else search (l++nextMyStates(S a x y z))

constructSolution (S a x y z) = if z==Null then [] else constructSolution z++[y]

checkgrid (a,b) ((c,d):ys) = if (a>3 || b>3 || c>3 || d>3) then False else if ys==[] then True else checkgrid (a,b) ys

solve (a,b) (y:ys) = if checkgrid (a,b) (y:ys) then constructSolution (search (nextMyStates (S (a,b) (y:ys) "" Null))) else ["please enter the correct positions in a 4 by 4 grid"]