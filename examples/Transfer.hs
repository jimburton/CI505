import System.IO

-- transfer contents of one file to another

main = do inp <- openFile "input.txt" ReadMode
          out <- openFile "out.txt" WriteMode
          str <- hGetContents inp
          hPutStr out str
          hClose inp
          hClose out

main2 = openFile "input.txt" ReadMode >>= \inp ->
          openFile "out.txt" WriteMode >>= (\out ->
            hGetContents inp >>= hPutStr out 
                             >> hClose out)
          >> hClose inp

{-main2 = openFile "input.txt" ReadMode >>= (\inp ->
          openFile "out.txt" WriteMode >>= (\out ->
           hGetContents inp >>= (\str -> hPutStr out str) 
                                  >> hClose) 
         >> hClose)
-}
