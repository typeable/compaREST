module Data.OpenApi.Compare.Common
  ( zipAll,
  )
where

zipAll :: [a] -> [b] -> Maybe [(a, b)]
zipAll [] [] = Just []
zipAll (x : xs) (y : ys) = ((x, y) :) <$> zipAll xs ys
zipAll (_ : _) [] = Nothing
zipAll [] (_ : _) = Nothing
