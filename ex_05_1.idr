printLonger : IO ()
printLonger = do putStr "First string: "
                 input1 <- getLine
                 let len1 = length input1
                 putStr "Second string: "
                 input2 <- getLine
                 let len2 = length input2
                 putStr (if len1 > len2 then (show len1) else (show len2))

printLonger2 : IO ()
printLonger2 = putStr "First string: " >>= \_ =>
               getLine >>= \input =>
               let len1 = length input in
               putStr "Second string: " >>= \_ =>
               getLine >>= \input2 =>
               let len2 = length input2 in
               putStr (if len1 > len2 then (show len1) else (show len2))
               

 
