module MyLib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

rev :: [a] -> [a]
rev [] = []
rev (x : xs) = rev xs ++ [x]

-- Напишите тесты к функции и функцию
--
-- Работает как zip, но если один список
-- длиннее, циклически переиспользует второй
--
-- > zipLong [1,2,3] "abc"
-- [(1,'a'),(2,'b'),(3,'c')]
--
-- > zipLong [1,2] "abcd"
-- [(1,'a'),(2,'b'),(1,'c'),(2,'d')]
--
-- > zipLong [] "abcd"
-- []
zipLong :: [a] -> [b] -> [(a, b)]
zipLong [] _ = []
zipLong _ [] = []
zipLong l1 l2 = zipAcc l1 l2 (max (length l1) (length l2))
  where
    zipAcc (x : _) (y : _) 1 = [(x, y)]
    zipAcc (x : xx) (y : yy) acc = (x, y) : zipAcc (xx ++ [x]) (yy ++ [y]) (acc - 1)

-- Binary Search Tree
--
-- left < root <= right
data Tree a
  = Empty
  | Node
      { left :: Maybe (Tree a),
        value :: a,
        right :: Maybe (Tree a)
      }
  deriving (Eq, Show, Read)


empty :: Tree a
empty = Empty

leaf :: a -> Tree a
leaf a = Node Nothing a Nothing

traversal :: Tree a -> [a]
traversal Empty = []
traversal (Node ml v mr) =
  maybe [] traversal ml ++ [v] ++ maybe [] traversal mr

insert :: (Ord a) => a -> Tree a -> Tree a
insert v Empty = leaf v
insert v t@(Node ml root mr)
  | v < root = t {left = Just $ maybe (leaf v) (insert v) ml}
  | otherwise = t {right = Just $ maybe (leaf v) (insert v) mr}

-- Напишите тесты-свойства к функциям и сами функции
-- левого и правого поворота деревьев
-- (см. https://en.wikipedia.org/wiki/Red%E2%80%93black_tree)
rotateLeft :: Tree a -> Tree a
rotateLeft (Node l v (Just (Node rl rv rr))) = Node (Just $ Node l v rl) rv rr
rotateLeft tree = tree

rotateRight :: Tree a -> Tree a
rotateRight (Node (Just (Node ll lv lr)) v r) = Node ll lv (Just $ Node lr v r)
rotateRight tree = tree
