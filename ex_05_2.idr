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
                                           
myRepl : String -> (String -> String) -> IO ()
myRepl x f = do putStr x
                input <- getLine
                putStrLn (f input)
                
myReplWith : (state : a) -> (prompt : String) -> (onInput : a -> String -> Maybe (String, a)) -> IO ()                
myReplWith state prompt onInput = do putStr prompt
                                     inp <- getLine
                                     let newState = onInput state inp
                                     case newState of
                                          Nothing => putStr "Exited"
                                          Just (p, s) => do putStrLn p
                                                            myReplWith s prompt onInput                      
                                     
main : IO ()
main = do random <- time
          guess (cast random)
