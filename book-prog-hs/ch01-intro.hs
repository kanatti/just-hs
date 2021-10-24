main :: IO ()
double :: Num a => a -> a
double x = x + x

{- HLINT ignore "Use foldr" -}
newSum :: Num p => [p] -> p
newSum [] = 0
newSum (n : ns) = n + newSum ns

newProduct :: Num p => [p] -> p
newProduct [] = 1
newProduct (n : ns) = n * newProduct ns

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [a | a <- xs, a > x]

seqn :: Monad m => [m a] -> m [a]
seqn [] = return []
seqn (act : acts) = do
  x <- act
  xs <- seqn acts
  return (x : xs)

rqsort :: Ord a => [a] -> [a]
rqsort [] = []
rqsort (x : xs) = rqsort larger ++ [x] ++ rqsort smaller
  where
    smaller = [a | a <- xs, a <= x]
    larger = [a | a <- xs, a > x]


uniqueSort :: Ord a => [a] -> [a]
uniqueSort [] = []
uniqueSort (x : xs) = uniqueSort smaller ++ [x] ++ uniqueSort larger
  where
    smaller = [a | a <- xs, a < x]
    larger = [a | a <- xs, a > x]

main = do
  print (double 2)
  print (newSum [1 .. 10])
  print (newProduct [2, 3, 4])
  print (qsort [1, 4, 2, 12, 5, 3])
  print (rqsort [1, 4, 2, 12, 5, 3])
  print (uniqueSort [2, 2, 3, 1, 1, 2])
  print "Hello World"