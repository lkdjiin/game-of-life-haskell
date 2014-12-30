module Slice (sliceAround) where

sliceAround :: Int -> [a] -> [a]
sliceAround 0 list = take 2 list
sliceAround n list = take 3 $ drop (n-1) list
