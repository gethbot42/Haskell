main = do
  putStrLn "Hello, what is your name?"
  name <- getLine
  putStrLn ("Hey, " ++ name ++ "!  It is nice to meet you.")