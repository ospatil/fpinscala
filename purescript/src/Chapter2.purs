module Chapter2 where

-- The operators are functions and can be imported individually
-- import Data.Ring ((-))
-- import Data.Semiring ((*))

import Prelude

import Data.Array (length, (!!))
import Data.Maybe (Maybe(..))

-- Listing 2.4.1
factorial :: Int -> Int
factorial n = go n 1
    where
        go 1 acc = acc
        go i acc = go (i - 1)(acc * i)

-- Exercise 2.1
fib :: Int -> Int
fib n = go n 0 1
    where
        go i acc curr
            | i == 0 = acc
            | otherwise = go (i - 1) curr (acc + curr)

-- Listing 2.4 - polymorphic function to find first elem in array
findFirst :: forall a. Array a -> (Maybe a -> Boolean) -> Maybe Int
findFirst arr p = loop 0
    where
        loop :: Int -> Maybe Int
        loop n
            | n >= length arr = Just (-1)
            | p (arr !! n) = Just n
            | otherwise = loop (n + 1)


