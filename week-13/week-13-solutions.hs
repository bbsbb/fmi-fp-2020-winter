-- Trees trees trees.

-- Recursive ADT's -
-- data Btree = Nil | Node Int Btree Btree    --

-- Cooler, parametrized ADT's

-- Ceci, est un arbre binaire! Oh mon dieu!
data Btree a = Nil | Node a (Btree a) (Btree a)
  deriving (Eq, Show, Read)

poopyTree = (Node 17 (Node 14 Nil (Node 7 Nil Nil)) (Node 20 Nil Nil))
poopyTree2 = (Node 17 (Node 14 Nil (Node 7 Nil Nil)) (Node 21 Nil Nil))


-- If you see trees, think pattern matching!

-- Exercise 1:  How do we find the depth of a tree?
treeDepth :: Btree a -> Int
treeDepth Nil = 0
treeDepth (Node _ l r) = 1 + max (treeDepth l) (treeDepth r)

-- Exercise 2: Count the number of leaves in a tree
countLeaves :: Btree a -> Int
countLeaves Nil = 0
countLeaves (Node _ l r) = 1 + (countLeaves l) + (countLeaves r)

-- Exercuse 3: Equality of binary trees?
-- Trees are equal if root + subtrees are equal.
treeEqual :: Eq a => Btree a -> Btree a -> Bool
treeEqual Nil Nil = True
treeEqual Nil _ = False
treeEqual _ Nil = False
treeEqual (Node root1 l1 r1) (Node root2 l2 r2) =  root1 == root2 &&  (treeEqual r1 r2) && (treeEqual l1 l2)

-- Exercise 4: Produce a flat structure traversing the tree in order.
-- In Order Traversal.
  ------------------------------------
  --       17                       --
  --     /   \                      --
  --    14   20                     --
  --  /  \                          --
  -- nil  8                         --
  -- margarita & sasho are right: in order: [14, 7, 17, 20] - Hail our student overlords!
------------------------------------
treeInOrder :: Btree a -> [a]
treeInOrder Nil = []
treeInOrder (Node root l r) = treeInOrder l ++ [root] ++ treeInOrder r


-- Exercise 5: Alright, find the deepest node containing an even element.


-- Zheni solution:

deepestEven :: Btree Int -> Int
deepestEven Nil = -1
deepestEven (Node root Nil Nil)
  | even root = 1
  | otherwise = -1
deepestEven (Node root Nil right) = 1 + deepestEven right
deepestEven (Node root left Nil) = 1 + deepestEven left
deepestEven (Node root left right) = 1 + max (deepestEven left) (deepestEven right)


-- Solution that ships in production:
deepestEvenInProd :: Btree Int -> Int
deepestEvenInProd Nil = -1000009 -- Scientifically derived number. Surely a tree can't be deeper?
deepestEvenInProd (Node root l r)
  | root `mod` 2 == 0  = max 0 ans
  | otherwise = ans
  where
    ansl = 1 + deepestEven l
    ansr = 1 + deepestEven r
    ans = max ansl ansr

-- Sasho optimized solution:
-- See above, set it to minus 2, remove 1's from where, wrap both cases of the guard in an if + 1. CAN YOU DO THAT, DEAR READER?
