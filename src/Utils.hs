module Utils where

holes :: [a] -> [(a, [a])]
holes []     = []
holes (x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <- holes xs]
