import Data.Vect

readToBlank : IO (List String)
readToBlank = do putStr "Enter something: "
                 x <- getLine
                 if (x == "")
                    then pure []
                    else do xs <- readToBlank
                            pure (x :: xs)
                            
stringer : (inputs : List String) -> String
stringer [] = ""
stringer (x :: xs) = x ++ "\n" ++ stringer xs

readAndSave : IO ()                            
readAndSave = do inputs <- readToBlank
                 putStr "Enter name of file: "
                 fileName <- getLine
                 Right h <- writeFile fileName (stringer inputs) | Left err => putStrLn (show err)
                 putStr "File written."

readLines : (handle : File) -> IO (n ** Vect n String)
readLines handle = do ended <- (fEOF handle)
                      case ended of
                           True => pure (_ ** [])
                           False => do Right x <- fGetLine handle | Left err => pure (_ ** [])
                                       (_ ** xs) <- readLines handle
                                       pure (_ ** x :: xs) 
                      

readVectFile : (filename : String) -> IO (n ** Vect n String)                 
readVectFile filename = do Right h <- openFile filename Read | Left err => pure (_ ** [])
                           (len ** vec) <- readLines h
                           closeFile h
                           pure (len ** vec)
