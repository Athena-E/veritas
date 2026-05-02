{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}
module FillWithOnes where

{-@ fillWithOnes :: n:Nat -> {xs:[Int] | len xs == n && AllEq 1 xs} @-}
fillWithOnes :: Int -> [Int]
fillWithOnes 0 = []
fillWithOnes n = 1 : fillWithOnes (n - 1)

{-@ measure AllEq @-}
AllEq :: Int -> [Int] -> Bool
AllEq _ [] = True
AllEq k (x:xs) = x == k && AllEq k xs

main :: IO ()
main = print (head xs + last xs)
  where
    xs = fillWithOnes 5
