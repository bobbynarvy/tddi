import System

readNumber : IO (Maybe Nat)
readNumber = do input <- getLine
                if all isDigit (unpack input)
                then pure (Just (cast input))
                else pure Nothing

guess : (target : Nat) -> IO ()
guess target = do putStr "Guess a number: "
                  Just num <- readNumber | Nothing => do putStr "Invalid input"
                                                         guess target
                  if num == target then do putStrLn "Correct!"
                                   else do putStrLn (if num > target 
                                                        then "Your guess is higher" 
                                                        else "Your guess is lower")
                                           guess target

main : IO ()
main = do random <- time
          guess (cast random)
