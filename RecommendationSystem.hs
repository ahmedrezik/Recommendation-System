import System.Random
import System.IO.Unsafe
randomZeroToX :: Int -> Int
randomZeroToX x= unsafePerformIO (getStdRandom (randomR (0, x)))


users = ["user1","user2","user3","user4"]
items = ["item1","item2","item3","item4","item5","item6"] 
purchasesHistory = [("user1",[["item1","item2","item3"],["item1","item2","item4"]]),
					("user2",[["item2","item5"],["item4","item5"]]),
					("user3",[["item3","item2"]]),
					("user4",[])]

----------------------------------createEmptyFreqList----------------------------
createEmptyFreqList :: [a] -> [(a, [b])]

createEmptyFreqList [x] = [(x,[])]
createEmptyFreqList (x:xs) =  [(x,[])] ++ createEmptyFreqList xs
------------------------------------getAllUsersStats-----------------------------

getAllUsersStats :: [(String, [[String]])] -> [(String, [(String, [(String, Int)])])]
getAllUsersStats list = getH list (genEmptyList purchasesHistory)

genEmptyList list = genH list []   
genH [] list = list 				
genH ((x,l):xs) list = genH xs (list ++ [(x,createEmptyFreqList items)]) 

getH [] list = list
getH ((x1,list1):xs) ((x2,list2):xs2) = (x2,(fill list1 list2)) : getH xs xs2

fill _ [] = [] 
fill list1 ((x1,list):xs) = (x1, list ++ countItems x1 (conc x1 list1)) : fill list1 xs 

countItems x1 list = countItemsHelper x1 list [] 
countItemsHelper _ [] list = list
countItemsHelper x1 (x:xs) list | x1 == x = countItemsHelper x1 (remove x (xs)) list
						        |otherwise = countItemsHelper x1 (remove x (xs)) 
												(list ++ [(x,count x (x:xs))])

remove x list = remH x list []
remH x [] list = list 
remH x (x1:xs) list | x==x1 = remH x xs list
					| otherwise = remH x xs (list ++ [x1])
				 
conc x nlist = concH x nlist []
concH x [] list = list 				 
concH x (x1:xs) list | member x x1 = concH x xs (list ++ x1)
					 | otherwise = concH x xs list

member :: (Eq a) => a -> [a] -> Bool 
member x [] = False
member x (x1:xs) | x==x1 = True
				 | otherwise = member x xs 
					 
count x list = countH x list 0
countH x [] num = num 
countH x (x1:xs) num | x==x1 = countH x xs (num+1)
					 | otherwise = countH x xs num
					 
distinct x = dH x []
dH [] list = list
dH (x:xs) list = dH xs (list++x) 
distinct1 x = dH1 (distinct x) []
dH1 [] list = list
dH1 (x:xs) list | member x xs = dH1 (remove x xs) (list ++ [x])
				| otherwise = dH1 xs (list ++ [x])
		
----------------------------------freqListItems-----------------------------------
freqListItems:: String -> [(String, Int)] 
freqListItems string = freqH purchasesHistory string

freqH ((x1,x2):xs) string = if x1==string then genlist x2
							else freqH xs string 

genlist nlist1 = genlistH items nlist1 []
genlistH [] _ list = list

genlistH (x:xs) nlist1 list = genlistH xs nlist1 (list ++ listH1 x nlist1 0 [])
listH1 x [] 0 list = []
listH1 x [] c list = list ++ [(x,c)] 
listH1 x (y:ys) c list = if member x y then listH1 x ys (c+length (remove x y)) list 
						else listH1 x ys c list
--------------------------------------freqListCart-----------------------------------
freqListCart:: String ->[String] -> [(String, Int)]

freqListCart string list = counts (freqListH purchasesHistory string list) []

freqListH [] _ _ = []
freqListH ((x1,x2):xs) string list = if x1==string then helper1 x2 list []
									else freqListH xs string list
helper1 [] _ a = a						
helper1 (x1:x2) list a = helper1 x2 list (a++helper2 x1 list [])   

helper2 _ [] a = a
helper2 x1 (x:xs) a = if member x x1 then helper2 x1 xs (a++ helper3 (remove x x1) [])
					else helper2 x1 xs a

helper3 [] a = a
helper3 (x:xs) [] = helper3 xs ([(x,1)])
helper3 (x:xs) ((x1,x2):x3) = if x==x1 then helper3 xs ((x1,(x2+1)):x3)
							else ((x1,x2): (helper3 (x:xs) x3))

counts [] a = a
counts (x:xs) a = if member x xs then counts (remove x xs) (a ++ (cH x xs 0))
				else counts xs (a++[x])

cH  (x,num) [] c = [(x,num+c)]
cH  x (x1:x2) c = if x==x1 then cH x x2 (c+1) else cH x x2 c 

--------------------------------freqListCartAndItems---------------------------------
freqListCartAndItems:: String -> [String] -> [(String, Int)]

freqListCartAndItems string list = freqListCartAndItemsH (freqListItems string) (freqListCart string list) []

freqListCartAndItemsH [] list acc = acc
freqListCartAndItemsH (x:xs) list acc = freqListCartAndItemsH xs (remove x list) (acc++(freqHelp x list [])) 

freqHelp (x,x1) [] acc = acc++[(x,x1)]
freqHelp (x,x1) ((y,y1):ys) acc = if x==y then acc++ [(x,(x1+y1))]
								else freqHelp (x,x1) ys acc
--------------------------------purchasesIntersection---------------------------------
--purchasesIntersection :: Eq a => [(a,[(a,Int)])] −> [(a,[(a,[(a,Int)])])] −> [[(a,[(a,Int)])]] 

itemIntersection (_, []) _ = []
itemIntersection _ (_, []) = []
itemIntersection (item1, (x : xs)) (item2, (y : ys)) = clearDuplicates items ((y : ys) ++ (x : xs))

itemsIntersection [] [] = []
itemsIntersection ((item1, list1) : xs) ((item2, list2) : ys) =
									remove (item1, []) ((item1, itemIntersection (item1, list1) (item2, list2)) : itemsIntersection xs ys)

purchasesIntersection _ [] = []
purchasesIntersection purchases (x : xs) = itemsIntersection purchases ((\(x, list) -> list) x) : purchasesIntersection purchases xs

clearDuplicates [] list = list
clearDuplicates (itm : itms) list = sumPairs (filter (\(x, _) -> x == itm) (list)) ++ clearDuplicates itms (removePair itm list)


removePair _ [] = []
removePair item ((a, b) : xs)
							| item == a = removePair item xs
							| otherwise = [(a, b)] ++ removePair item xs
							
sumFreq [] = 0
sumFreq ((a, num) : xs) = num + sumFreq xs
sumPairs [] = []
sumPairs ((a, num) : xs) = [(a, sumFreq ((a, num) : xs))]							












count_Items [] = [("",0)]
count_Items (x:xs) =
	(x,countItem x (x:xs)) : (count_Items (remove x (x:xs)))

countItem a [] = 0
countItem a (x:xs) =
	if a==x then 1 + countItem a xs
	else countItem a xs

-----------------------------------------recommendEmptyCart-----------------------------
recommendEmptyCart :: String -> String

recommendEmptyCart x = if length (helper212(freqListItems x))/=0 then (helper212 (freqListItems x) )!! randomZeroToX ((length (helper212 (freqListItems x) ))-1) else ""

-------------------------------------recommendBasedOnItemsInCart-------------------------
recommendBasedOnItemsInCart :: String -> [String] -> String

recommendBasedOnItemsInCart x cart = if length((helper212 (freqListCartAndItems x cart) ))/=0 then (helper212 (freqListCartAndItems x cart) )!! randomZeroToX ((length (helper212 (freqListCartAndItems x cart) ))-1) else ""

helper212 [] = []
helper212 ((item,n):xs) = if n>0 then [item]++ helper212 ((item,n-1):xs) else helper212 xs

-----------------------------------------------recommendBasedOnUsers-----------------------
recommendBasedOnUsers :: String -> String

recommendBasedOnUsers user = if length(helper212(freqListUsers user))/=0 then (helper212 (freqListUsers user) )!! randomZeroToX ((length(helper212 (freqListUsers user)))-1) else ""


-----------------------------------------------recommend-----------------------------------
-- recommend :: String -> [String] -> String

-- recommend user cart = if cartRecommendation /= "" && userRecommendation /= ""
						-- then ([cartRecommendation]++[userRecommendation])!! randomZeroToX(1) -- recommend from both
						-- else if cartRecommendation == "" && userRecommendation == ""
							-- then randomZeroToX(length(items)-1)            -- randomly recommend an item
							-- else if cartRecommendation == [] then userRecommendation
															-- else cartRecommendation
					-- where 
						-- userRecommendation = recommendBasedOnUsers user
						-- cartRecommendation = if length cart /=0 
												-- then recommendBasedOnItemsInCart user cart 
												-- else recommendEmptyCart user 
	
												
------------------------------------freqListUsers--------------------------------------
freqListUsers:: String -> [(String, Int)]	
			
freqListUserhelper ([]:xs) = freqListUserhelper xs
freqListUserhelper (((x,y):t):xs) = y ++ freqListUserhelper (t:xs)
freqListUserhelper (([])) = []



countTup _ [] = 0
countTup a ((x,y):xs) = if a== x then y + countTup a xs else countTup a xs


countFinalTip [] _ = [("",0)]
countFinalTip (y:ys) (x:xs) = (y,countTup y (x:xs)) : countFinalTip ys (x:xs) 


removeuser a []=[]
removeuser a ((x,(y:ys)):xs)= if a==x then y:ys else removeuser  a xs

containZero [] = []
containZero ((x,y):xs) = if y==0 then (x,y) : containZero xs else containZero xs

removeZero [] l = l
removeZero (y:ys) ((x,y1):xs)  =   removeZero ys (remove y ((x,y1):xs))

removeuser1 a []=[]
removeuser1 a ((x,(y:ys)):xs)= if a==x then xs else (x,(y:ys)) : removeuser1  a xs

freqListUsers a = removeZero (containZero(countFinalTip (items) (freqListUserhelper ((purchasesIntersection (removeuser a (getAllUsersStats (purchasesHistory))) (removeuser1 a (getAllUsersStats (purchasesHistory)))))))) (countFinalTip (items) (freqListUserhelper ((purchasesIntersection (removeuser a (getAllUsersStats (purchasesHistory))) (removeuser1 a (getAllUsersStats (purchasesHistory)))))))