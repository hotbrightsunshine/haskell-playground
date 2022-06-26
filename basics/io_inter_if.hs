main = do
  let arr = ["Simon", "John", "Phil"]
  putStrLn "Inserisci il tuo nome: "
  nome <- getLine

  if nome `elem` arr then
    do putStrLn "Haskell è un bel linguaggio. "
       else if nome == "Koen" then
              do putStrLn "Mi piace il debugging."
            else do putStrLn "Non ti conosco. "
              
