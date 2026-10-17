module Common.Heap where

-- 共通: 重複を保持できる leftist min-heap。Down を使うと max-heap。
data Heap a = HEmpty | HNode !Int !Int a (Heap a) (Heap a)
  deriving (Show)

heapRank :: Heap a -> Int
heapRank HEmpty = 0
heapRank (HNode r _ _ _ _) = r

heapSize :: Heap a -> Int
heapSize HEmpty = 0
heapSize (HNode _ n _ _ _) = n

heapNode :: a -> Heap a -> Heap a -> Heap a
heapNode x a b
  | heapRank a < heapRank b = heapNode x b a
  | otherwise = HNode (heapRank b + 1) (1 + heapSize a + heapSize b) x a b

meld :: (Ord a) => Heap a -> Heap a -> Heap a
meld HEmpty b = b
meld a HEmpty = a
meld a@(HNode _ _ x l r) b@(HNode _ _ y _ _)
  | x <= y = heapNode x l (meld r b)
  | otherwise = meld b a

heapPush :: (Ord a) => a -> Heap a -> Heap a
heapPush x = meld (HNode 1 1 x HEmpty HEmpty)

heapPeek :: Heap a -> Maybe a
heapPeek HEmpty = Nothing
heapPeek (HNode _ _ x _ _) = Just x

heapPop :: (Ord a) => Heap a -> Maybe (a, Heap a)
heapPop HEmpty = Nothing
heapPop (HNode _ _ x l r) = Just (x, meld l r)
