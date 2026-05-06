{-@ LIQUID "--ple" @-}
module SafeMidpoint where

{-@ safeMidpoint :: lo:Int -> hi:{Int | lo <= hi} -> {m:Int | lo <= m && m <= hi} @-}
safeMidpoint :: Int -> Int -> Int
safeMidpoint lo hi = lo + ((hi - lo) `div` 2)

main :: IO ()
main = print (safeMidpoint 10 20)
