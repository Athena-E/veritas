{-@ LIQUID "--ple" @-}
module BoundedReadOffset where

{-@ readOffset :: i:{Int | 0 <= i && i < 4} -> {v:Int | v == 7} @-}
readOffset :: Int -> Int
readOffset i = arr !! (i + 1)
  where
    arr = [7, 7, 7, 7, 7]

main :: IO ()
main = print (readOffset 2)
