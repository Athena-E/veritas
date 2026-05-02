{-@ LIQUID "--ple" @-}
module SafeDivision where

{-@ safeDivide :: a:Int -> b:{Int | b /= 0} -> Int @-}
safeDivide :: Int -> Int -> Int
safeDivide a b = a `div` b

main :: IO ()
main = print (safeDivide 10 2)
