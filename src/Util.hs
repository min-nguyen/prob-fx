{- | Some small utility functions -}

module Util (
    boolToInt
  , safeHead
  , safeTail
  , findIndexes) where

-- | Return @True@ for @1@ and otherwise @False@
boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

-- | Safely attempt to return the head of a list
safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

-- | Return the tail of a list, behaving as the identity function upon an empty list
safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

-- | Return all the positions that a value occurs within a list
findIndexes :: Eq a => [a] -> a -> [Int]
findIndexes xs a = reverse $ go xs 0 []
  where
  go (x:xs) i inds = if x == a then go xs (i + 1) (i:inds)
                     else go xs (i + 1) inds
  go [] i inds = inds