

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else [x*x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else []

twiceIfEvenFn :: Integer -> [Integer]
twiceIfEvenFn x = if even x then [x*x, x*x] else []

twiceWhenEven'' :: [Integer] -> [Integer]
twiceWhenEven'' xs = xs >>= twiceIfEvenFn

