module Main where

import Data.Char

{- | A função 'outStr' é uma função que dado o mapa em formato txt, ou seja, quando visto em forma de String, separado por '/n', corta por este mesmo separador em várias String's.
-}

outStr :: [String] -> String
outStr [] = "\n"
outStr t = unlines t

{- | A função 'main' é a função predefinida na linguagem de programação Haskell como o ponto de entrada de um programa.
-}

main = do inp <- getContents
          putStr (outStr (tarefa3 (lines inp)))

tarefa3 :: [String] -> [String]
tarefa3 txt = [msg]
    where tab = takeWhile (all isAlpha) txt
          linhaCoord = take 1 (drop (length tab) txt)
          prog = (drop (length tab + 1) txt) !! 0
          coordX = read (head (words (head linhaCoord))) :: Int
          coordY = read (head (tail (words (head linhaCoord)))) :: Int
          orient = last (words (head linhaCoord))
          comando = (head (drop (length tab + 1) txt)) !! 0
          posInicialTab = (head (take 1 (drop (length tab - 1 - coordY) txt))) !! coordX
          proxiPosTab | (comando == 'A') && (orient == "N") = (head (take 1 (drop (length tab - 1 - coordY - 1) txt))) !! coordX
                      | (comando == 'A') && (orient == "E") = (head (take 1 (drop (length tab - 1 - coordY) txt))) !! (coordX + 1)
                      | (comando == 'A') && (orient == "O") = (head (take 1 (drop (length tab - 1 - coordY) txt))) !! (coordX - 1)
                      | (comando == 'A') && (orient == "S") = (head (take 1 (drop (length tab - 1 - coordY + 1) txt))) !! coordX
                      | (comando == 'S') && (orient == "N") = (head (take 1 (drop (length tab - 1 - coordY - 1) txt))) !! coordX
                      | (comando == 'S') && (orient == "E") = (head (take 1 (drop (length tab - 1 - coordY) txt))) !! (coordX + 1)
                      | (comando == 'S') && (orient == "O") = (head (take 1 (drop (length tab - 1 - coordY) txt))) !! (coordX - 1)
                      | (comando == 'S') && (orient == "S") = (head (take 1 (drop (length tab - 1 - coordY + 1) txt))) !! coordX
                      | (comando == 'E') && (orient == "N") = (head (take 1 (drop (length tab - 1 - coordY) txt))) !! coordX
                      | (comando == 'E') && (orient == "E") = (head (take 1 (drop (length tab - 1 - coordY) txt))) !! coordX
                      | (comando == 'E') && (orient == "O") = (head (take 1 (drop (length tab - 1 - coordY) txt))) !! coordX
                      | (comando == 'E') && (orient == "S") = (head (take 1 (drop (length tab - 1 - coordY) txt))) !! coordX
                      | (comando == 'D') && (orient == "N") = (head (take 1 (drop (length tab - 1 - coordY) txt))) !! coordX
                      | (comando == 'D') && (orient == "E") = (head (take 1 (drop (length tab - 1 - coordY) txt))) !! coordX
                      | (comando == 'D') && (orient == "O") = (head (take 1 (drop (length tab - 1 - coordY) txt))) !! coordX
                      | (comando == 'D') && (orient == "S") = (head (take 1 (drop (length tab - 1 - coordY) txt))) !! coordX
                      | (comando == 'L') && (orient == "N") = (head (take 1 (drop (length tab - 1 - coordY) txt))) !! coordX
                      | (comando == 'L') && (orient == "E") = (head (take 1 (drop (length tab - 1 - coordY) txt))) !! coordX
                      | (comando == 'L') && (orient == "O") = (head (take 1 (drop (length tab - 1 - coordY) txt))) !! coordX
                      | (comando == 'L') && (orient == "S") = (head (take 1 (drop (length tab - 1 - coordY) txt))) !! coordX
          msg = executaComandos prog 

executaComandos :: String -> String -> Int -> Int -> Char -> Char -> [String] -> [String]
executaComandos (h:t) orient coordX coordY posInicialTab proxiPosTab txt = (validaComandos h orient coordX coordY posInicialTab proxiPosTab txt):(validaComandos t orient' coordX' coordY' posInicialTab' proxiPosTab' txt)
  where coordX' = read ((words (validaComandos h orient coordX coordY posInicialTab proxiPosTab)) !! 0)
        coordY' = read ((words (validaComandos h orient coordX coordY posInicialTab proxiPosTab)) !! 1)
        orient' = (words (validaComandos h orient coordX coordY posInicialTab proxiPosTab)) !! 2
        posInicialTab' = proxiPosTab
        proxiPosTab' | (comando == 'A') && (orient == "N") = (head (take 1 (drop (length txt - 3 - coordY - 1) txt))) !! coordX
                     | (comando == 'A') && (orient == "E") = (head (take 1 (drop (length txt - 3 - coordY) txt))) !! (coordX + 1)
                     | (comando == 'A') && (orient == "O") = (head (take 1 (drop (length txt - 3 - coordY) txt))) !! (coordX - 1)
                     | (comando == 'A') && (orient == "S") = (head (take 1 (drop (length txt - 3 - coordY + 1) txt))) !! coordX
                     | (comando == 'S') && (orient == "N") = (head (take 1 (drop (length txt - 3 - coordY - 1) txt))) !! coordX
                     | (comando == 'S') && (orient == "E") = (head (take 1 (drop (length txt - 3 - coordY) txt))) !! (coordX + 1)
                     | (comando == 'S') && (orient == "O") = (head (take 1 (drop (length txt - 3 - coordY) txt))) !! (coordX - 1)
                     | (comando == 'S') && (orient == "S") = (head (take 1 (drop (length txt - 3 - coordY + 1) txt))) !! coordX
                     | (comando == 'E') && (orient == "N") = (head (take 1 (drop (length txt - 3 - coordY) txt))) !! coordX
                     | (comando == 'E') && (orient == "E") = (head (take 1 (drop (length txt - 3 - coordY) txt))) !! coordX
                     | (comando == 'E') && (orient == "O") = (head (take 1 (drop (length txt - 3 - coordY) txt))) !! coordX
                     | (comando == 'E') && (orient == "S") = (head (take 1 (drop (length txt - 3 - coordY) txt))) !! coordX
                     | (comando == 'D') && (orient == "N") = (head (take 1 (drop (length txt - 3 - coordY) txt))) !! coordX
                     | (comando == 'D') && (orient == "E") = (head (take 1 (drop (length txt - 3 - coordY) txt))) !! coordX
                     | (comando == 'D') && (orient == "O") = (head (take 1 (drop (length txt - 3 - coordY) txt))) !! coordX
                     | (comando == 'D') && (orient == "S") = (head (take 1 (drop (length txt - 3 - coordY) txt))) !! coordX
                     | (comando == 'L') && (orient == "N") = (head (take 1 (drop (length txt - 3 - coordY) txt))) !! coordX
                     | (comando == 'L') && (orient == "E") = (head (take 1 (drop (length txt - 3 - coordY) txt))) !! coordX
                     | (comando == 'L') && (orient == "O") = (head (take 1 (drop (length txt - 3 - coordY) txt))) !! coordX
                     | (comando == 'L') && (orient == "S") = (head (take 1 (drop (length txt - 3 - coordY) txt))) !! coordX
        comando = t


validaComandos :: Char -> String -> Int -> Int -> Char -> Char -> [String] -> String
validaComandos comando orient coordX coordY posInicialTab proxiPosTab txt | (comando == 'A') = validaAvancar orient coordX coordY posInicialTab proxiPosTab txt
                                                                          | (comando == 'S') = validaSaltar orient coordX coordY posInicialTab proxiPosTab txt
                                                                          | (comando == 'D') = validaDireita orient coordX coordY txt
                                                                          | (comando == 'E') = validaEsquerda orient coordX coordY txt
                                                                          | (comando == 'L') = validaLuz orient coordX coordY posInicialTab txt


validaSaltar :: String -> Int -> Int -> Char -> Char -> [String] -> String
validaSaltar orient coordX coordY posInicialTab proxiPosTab txt = if ((orient == "O") && (coordX > 0) && (ord (posInicialTab) + 1 == ord (proxiPosTab) || ord (proxiPosTab) < ord (posInicialTab))) || ((orient == "O") && (coordX > 0) && (ord (toLower (posInicialTab)) + 1 == ord (toLower (proxiPosTab)) || ord (toLower (proxiPosTab)) < ord (toLower (posInicialTab))))
                                                                  then ((show (coordX - 1))++" "++(show (coordY))++" "++"O")
                                                                  else if ((orient == "N") && (coordY < (length txt - 2 - 1)) && (ord (posInicialTab) + 1 == ord (proxiPosTab) || ord (proxiPosTab) < ord (posInicialTab))) || ((orient == "N") && (coordY < (length txt - 2 - 1)) && (ord (toLower (posInicialTab)) + 1 == ord (toLower (proxiPosTab)) || ord (toLower (proxiPosTab)) < ord (toLower (posInicialTab))))
                                                                       then ((show (coordX))++" "++(show (coordY + 1))++" "++"N")
                                                                       else if ((orient == "E") && (coordX < (length (head txt)) - 1) && (ord (posInicialTab) + 1 == ord (proxiPosTab) || ord (proxiPosTab) < ord (posInicialTab))) || ((orient == "E") && (coordX < (length (head txt)) - 1) && (ord (toLower (posInicialTab)) + 1 == ord (toLower (proxiPosTab)) || ord (toLower (proxiPosTab)) < ord (toLower (posInicialTab))))
                                                                            then ((show (coordX + 1))++" "++(show (coordY))++" "++"E")
                                                                            else if ((orient == "S") && (coordY > 0) && (ord (posInicialTab) + 1 == ord (proxiPosTab) || ord (proxiPosTab) < ord (posInicialTab))) || ((orient == "S") && (coordY > 0) && (ord (toLower (posInicialTab)) + 1 == ord (toLower (proxiPosTab)) || ord (toLower (proxiPosTab)) < ord (toLower (posInicialTab))))
                                                                                 then ((show (coordX))++" "++(show (coordY - 1))++" "++"S")
                                                                                 else ((show (coordX))++" "++(show (coordY))++" "++orient)


validaAvancar :: String -> Int -> Int -> Char -> Char -> [String] -> String
validaAvancar orient coordX coordY posInicialTab proxiPosTab txt = if (orient == "O") && (coordX > 0) && (ord (posInicialTab) == ord (proxiPosTab)) || (orient == "O") && (coordX > 0) && ord (toLower (posInicialTab)) == ord (toLower (proxiPosTab))
                                                                   then ((show (coordX - 1))++" "++(show (coordY))++" "++"O")
                                                                   else if (orient == "N") && (coordY < (length txt - 2 - 1)) && (ord (posInicialTab) == ord (proxiPosTab)) || (orient == "N") && (coordY < (length txt - 2 - 1)) && (ord (toLower posInicialTab) == ord (toLower proxiPosTab))
                                                                        then ((show (coordX))++" "++(show (coordY + 1))++" "++"N")
                                                                        else if (orient == "E") && (coordX < (length (head txt) - 1)) && (ord (posInicialTab) == ord (proxiPosTab)) || (orient == "E") && (coordX < (length (head txt) - 1)) && (ord (toLower posInicialTab) == ord (toLower proxiPosTab))
                                                                             then ((show (coordX + 1))++" "++(show (coordY))++" "++"E")
                                                                             else if (orient == "S") && (coordY > 0) && (ord (posInicialTab) == ord (proxiPosTab)) || (orient == "S") && (coordY > 0) && (ord (toLower posInicialTab) == ord (toLower proxiPosTab))
                                                                                  then ((show (coordX))++" "++(show (coordY - 1))++" "++"S")
                                                                                  else ((show (coordX))++" "++(show (coordY))++" "++orient)


validaLuz :: String -> Int -> Int -> Char -> [String] -> String
validaLuz orient coordX coordY posInicialTab txt = if (orient == "O") && posInicialTab >= 'A' && posInicialTab <= 'Z'
                                                   then ((show (coordX))++" "++(show (coordY))++" "++"O")
                                                   else if (orient == "N") && posInicialTab >= 'A' && posInicialTab <= 'Z'
                                                        then ((show (coordX))++" "++(show (coordY))++" "++"N")
                                                        else if (orient == "E") && posInicialTab >= 'A' && posInicialTab <= 'Z'
                                                             then ((show (coordX))++" "++(show (coordY))++" "++"E")
                                                             else if (orient == "S") && posInicialTab >= 'A' && posInicialTab <= 'Z'
                                                                  then ((show (coordX))++" "++(show (coordY))++" "++"S")
                                                                  else ((show (coordX))++" "(show (coordY))++" "++orient)


validaDireita :: String -> Int -> Int -> [String] -> String
validaDireita orient coordX coordY txt = if (orient == "O")
                                         then ((show (coordX))++" "++(show (coordY))++" "++"N")
                                         else if (orient == "N")
                                              then ((show (coordX))++" "++(show (coordY))++" "++"E")
                                              else if (orient == "E")
                                                   then ((show (coordX))++" "++(show (coordY))++" "++"S")
                                                   else if (orient == "S")
                                                        then ((show (coordX))++" "++(show (coordY))++" "++"O")
                                                        else ((show (coordX))++" "++(show (coordY))++" "++orient)


validaEsquerda :: String -> Int -> Int -> [String] -> String
validaEsquerda orient coordX coordY txt = if (orient == "O")
                                         then ((show (coordX))++" "++(show (coordY))++" "++"S")
                                         else if (orient == "N")
                                              then ((show (coordX))++" "++(show (coordY))++" "++"O")
                                              else if (orient == "E")
                                                   then ((show (coordX))++" "++(show (coordY))++" "++"N")
                                                   else if (orient == "S")
                                                        then ((show (coordX))++" "++(show (coordY))++" "++"E")
                                                        else ((show (coordX))++" "++(show (coordY))++" "++orient)