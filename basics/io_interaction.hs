main = do
  putStrLn "Base: "
  baseStr <- getLine
  let base = read baseStr :: Float
  putStrLn "Altezza: "
  altzStr <- getLine
  let altz = read altzStr :: Float
  let area = base * altz
  putStr "Altezza: "
  print area
