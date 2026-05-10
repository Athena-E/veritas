{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}
module SortedHead where

{-@ reflect sorted @-}
sorted :: [Int] -> Bool
sorted [] = True
sorted (x:xs) =
  case xs of
    [] -> True
    (y:_) -> x <= y && sorted xs

{-@ sortedHead :: xs:{[Int] | len xs == 3 && sorted xs} -> Int @-}
sortedHead :: [Int] -> Int
sortedHead (x:_) = x
sortedHead [] = 0

{-@ makeSorted :: {xs:[Int] | len xs == 3 && sorted xs} @-}
makeSorted :: [Int]
makeSorted = [1, 2, 3]

main :: IO ()
main = print (sortedHead makeSorted)
