{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}
{-@ LIQUID "--no-termination" @-}
module BinarySearch where

{-@ reflect sorted @-}
sorted :: [Int] -> Bool
sorted [] = True
sorted (x:xs) =
  case xs of
    [] -> True
    (y:_) -> x <= y && sorted xs

{-@ midpoint :: lo:Int -> hi:{Int | lo <= hi} -> {m:Int | lo <= m && m <= hi} @-}
midpoint :: Int -> Int -> Int
midpoint lo hi = lo + ((hi - lo) `div` 2)

{-@ binarySearch :: xs:{[Int] | len xs == 10 && sorted xs} -> Int -> Int @-}
binarySearch :: [Int] -> Int -> Int
binarySearch xs target = go 0 9
  where
    {-@ go :: lo:{Int | 0 <= lo && lo <= 10}
           -> hi:{Int | -1 <= hi && hi < 10 && lo <= hi + 1}
           -> Int
      @-}
    go :: Int -> Int -> Int
    go lo hi
      | lo > hi = -1
      | otherwise =
          let mid = midpoint lo hi
              val = xs !! mid
          in if val == target
                then mid
                else if val < target
                        then go (mid + 1) hi
                        else go lo (mid - 1)

main :: IO ()
main = print (binarySearch [3,7,12,15,22,34,41,55,68,90] 34)
