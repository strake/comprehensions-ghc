{-# OPTIONS_GHC -fplugin=Comprehension.Plugin #-}
{-# LANGUAGE ApplicativeDo, RecursiveDo #-}

module Main where

main :: IO ()
main = test1 pure ()

test1 :: Functor f => (a -> f b) -> a -> f b
test1 f a = [b | b <- f a]

test2 :: Applicative p => (a -> p x) -> (a -> p y) -> a -> p (x, y)
test2 f g a = [(x, y) | x <- f a, y <- g a]
