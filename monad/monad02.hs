import Control.Applicative ((*>))

sequencing :: IO ()
sequencing = do
  putStrLn "Hello"
  putStrLn "World"

sequencing' :: IO ()
sequencing' =
  putStrLn "Hello" >> 
  putStrLn "World"

sequencing'' :: IO ()
sequencing'' =
  putStrLn "Hello" *> 
  putStrLn "World"

binding :: IO ()
binding = do
  name <- getLine
  putStrLn name

binding' :: IO ()
binding' =
  getLine >>= putStrLn

bindandsequencing :: IO ()
bindandsequencing = do
  putStrLn "Name Pls :"
  name <- getLine
  putStrLn ("Hello " ++ name)

bindandsequencing' :: IO ()
bindandsequencing' =
  putStrLn "Name Pls :" >>
  getLine >>=
  \name -> putStrLn ("Hello " ++ name)

twoBinds :: IO ()
twoBinds = do
  putStrLn "Name pls:"
  name <- getLine
  putStrLn "Age ? :"
  age <- getLine
  putStrLn ("Hello " ++ name ++ " " ++ age)

twoBinds' :: IO ()
twoBinds' =
  putStrLn "Name pls:" >>
  getLine >>=
  \name ->
  putStrLn "Age ? :" >>
  getLine >>=
  \age ->
  putStrLn ("Hello " ++ name ++ " " ++ age)

