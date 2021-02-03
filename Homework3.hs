--(a)
data MultiTree a b = MLeaf a | MNode b (MultiTree a b) (MultiTree a b) deriving Show
-- Defining the data type of the tree and the haskell value of the tree
myTree :: MultiTree String Int
myTree = MNode 150 ( MNode 17 (MLeaf "a") (MLeaf "bcd")) (MNode 29 (MLeaf "de") (MLeaf "fghi"))


--(b)
-- The function takes a function and a tree as arguments and returns back tree after the function is applied to its leaves.
leafMap :: (String -> Int) -> MultiTree String Int -> MultiTree Int Int
leafMap f (MNode n tree1 tree2) = MNode n (leafMap f tree1) (leafMap f tree2)
leafMap f (MLeaf v) = (MLeaf (f v) )


-- (c)
-- The function takes a function and a tree as arguments and returns back tree after the function is applied to its nodes.
nodeMap :: (Int -> Int) -> MultiTree String Int -> MultiTree String Int
nodeMap f (MNode n tree1 tree2) = MNode (f n) (nodeMap f tree1) (nodeMap f tree2)
nodeMap f (MLeaf v) = (MLeaf v)


-- (e)
-- A function to traverse the tree depth first.
multiTreeDFT :: MultiTree a b -> [Either a b]
multiTreeDFT (MLeaf v) = [Left v]
multiTreeDFT (MNode a tree1 tree2) = [Right a] ++ multiTreeDFT tree1 ++ multiTreeDFT tree2

