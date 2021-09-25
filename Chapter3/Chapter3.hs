module Chapter3.Chapter3 where

flip :: (a -> b -> c) -> (b -> a -> c)
flip f a b = f b a

curry :: ((a, b) -> c) -> (a -> b -> c)
curry f a b = f (a, b)

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f args = f (fst args) (snd args)