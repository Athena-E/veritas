{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}
module BinarySearch where

{-@ measure sorted @-}
sorted :: [Int] -> Bool
sorted [] = True
sorted [_] = True
sorted (x:y:xs) = x <= y && sorted (y:xs)

{-@ binarySearch :: xs:{[Int] | len xs == 10 && sorted xs} -> Int -> Int @-}
binarySearch :: [Int] -> Int -> Int
binarySearch xs target = go 0 (length xs - 1)
  where
    go lo hi
      | lo > hi = -1
      | otherwise =
          let mid = lo + ((hi - lo) `div` 2)
              val = xs !! mid
          in if val == target
                then mid
                else if val < target
                        then go (mid + 1) hi
                        else go lo (mid - 1)

main :: IO ()
main = print (binarySearch [3,7,12,15,22,34,41,55,68,90] 34)
