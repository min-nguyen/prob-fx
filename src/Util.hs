{- | Some minor utility functions -}

module Util (
    safeHead
  , safeTail) where

-- | Safely attempt to return the head of a list
safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

-- | Return the tail of a list, behaving as the identity function upon an empty list
safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs
