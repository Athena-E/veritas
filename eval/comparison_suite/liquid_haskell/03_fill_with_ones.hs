{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}
module FillWithOnes where

{-@ fillWithOnes :: n:Nat -> {xs:[Int] | len xs == n && allEq 1 xs} @-}
fillWithOnes :: Int -> [Int]
fillWithOnes 0 = []
fillWithOnes n = 1 : fillWithOnes (n - 1)

{-@ reflect allEq @-}
allEq :: Int -> [Int] -> Bool
allEq _ [] = True
allEq k (x:xs) = x == k && allEq k xs

main :: IO ()
main = print (head xs + last xs)
  where
    xs = fillWithOnes 5
