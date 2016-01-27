data Tree a = Empty | Node a (Tree a) (Tree a)

exampleTree :: Tree String
exampleTree = Node "F" 
                (Node "B" 
                  (Node "A" Empty Empty) 
                  (Node "D"
                    (Node "C" Empty Empty)
                    (Node "E" Empty Empty))) 
                (Node "G"
                  Empty 
                  (Node "I" 
                    (Node "H" Empty Empty) Empty))

					
treeElementsCount :: Tree a -> Int 
treeElementsCount Empty = 0
treeElementsCount (Node value left right) = 1 + (treeElementsCount left) + (treeElementsCount right)
-- връща броя на възлите в едно дърво
treeHeight :: Tree a -> Int -- връща височината на едно дърво
treeHeight Empty = 0
treeHeight (Node value left right) = 1 + max (treeHeight left) (treeHeight right)

flattenTree :: Tree a -> [a] -- връща дървото, изгладено до списък. Нека обхождането да бъде ляво-корен-дясно
flattenTree Empty = []
flattenTree (Node value left right) = flattenTree (left) ++ (value:[]) ++ flattenTree (right)

treeFind :: Eq a => a -> Tree a -> Bool 
treeFind _ Empty = False 
treeFind x tree = isMember x $ flattenTree tree
	where 
		isMember _ [] = False
		isMember x (y:ys)
			| x == y = True 
			| otherwise = isMember x ys
			

instance Show a => Show (Tree a) where
  show tree = show $ flattenTree tree
  
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f Empty = Empty
treeMap f (Node v l r) = Node (f v) (treeMap f l) (treeMap f r)

bstFind :: Ord a => a -> Tree a -> Bool 
bstFind _ Empty = False
bstFind x (Node v l r) 
	| x == v = True
	| x < v = bstFind x l
	| otherwise = bstFind x r

  