double :: Num a => a -> a
double x = x + x

quadruple :: Num a => a -> a
quadruple x = double (double x)

factorial :: (Num a, Enum a) => a -> a
factorial n = product [1 .. n]

average :: Foldable t => t Int -> Int
average ns = sum ns `div` length ns

n = a `div` length xs
  where
    a = 10
    xs = [1, 2, 3, 4, 5]

last1 :: [a] -> a
last1 [] = error "Empty List"
last1 [x] = x
last1 (x : xs) = last1 xs

last2 :: [a] -> a
last2 xs = xs !! (length xs - 1)

init1 :: [a] -> [a]
init1 [] = error "Empty list"
init1 [x] = []
init1 (x : xs) = x : init1 xs

init2 :: [a] -> [a]
init2 xs = take (length xs - 1) xs
