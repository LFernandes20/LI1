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
          putStr (outStr (tarefa2 (lines inp)))


{- | A função 'tarefa2' irá permitirnos averiguar se o primeiro comando dado é ou não aplicável, tendo em conta 
que o mapa recebido como input é válido.
Caso seja aplicável, o output será a Linha da Posição Inicial e Orientação após se efetuar o comando, caso contrário 
o output será ["ERRO"].
-}


tarefa2 :: [String] -> [String]
tarefa2 txt = [msg]
    where tab = takeWhile (all isAlpha) txt
          linhaCoord = take 1 (drop (length tab) txt)
          prog = drop (length tab + 1) txt
          coordX = read (head (words (head linhaCoord))) :: Int
          coordY = read (head (tail (words (head linhaCoord)))) :: Int
          orient = last (words (head linhaCoord))
          comando = (head (drop (length tab + 1) txt)) !! 0
          posInicialTab = (head (take 1 (drop (length tab - 1 - coordY) txt))) !! coordX
          posFinalTab | (comando == 'A') && (orient == "N") = (head (take 1 (drop (length tab - 1 - coordY - 1) txt))) !! coordX
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
          msg | comando == 'A' = validaAvancar orient coordX coordY posInicialTab posFinalTab txt
              | comando == 'S' = validaSaltar orient coordX coordY posInicialTab posFinalTab txt
              | comando == 'D' = validaDireita orient coordX coordY txt
              | comando == 'E' = validaEsquerda orient coordX coordY txt
              | comando == 'L' = validaLuz orient coordX coordY posInicialTab txt


-- ** Secção de Funções Auxiliares
-- Funções usadas para definir a função principal.

{- | A função 'validaAvancar' avalia se é ou não possível efetuar o comando "Avançar" em qualquer caso atribuído, para isso, certas condições deverão ser seguidas:

*O robot ao avançar não pode sair fora do tabuleiro em causa;

*Sabendo que o tabuleiro de jogo é constituído por letras, maiúsculas e minúsculas, o robot só avança de uma posição para outra se a letra for igual, independentemente de ser
maiúscula ou minúscula, ou seja, de um 'a' para um 'a', de um 'b' para um 'b' ou então p.e. de um 'a' para um 'A'.
-}


validaAvancar :: String -> Int -> Int -> Char -> Char -> [String] -> String
validaAvancar orient coordX coordY posInicialTab posFinalTab txt = if (orient == "O") && (coordX > 0) && (ord (posInicialTab) == ord (posFinalTab)) || (orient == "O") && (coordX > 0) && ord (toLower (posInicialTab)) == ord (toLower (posFinalTab))
                                                                  then ((show (coordX - 1))++" "++(show (coordY))++" "++"O")
                                                                  else if (orient == "N") && (coordY < (length txt - 2 - 1)) && (ord (posInicialTab) == ord (posFinalTab)) || (orient == "N") && (coordY < (length txt - 2 - 1)) && (ord (toLower posInicialTab) == ord (toLower posFinalTab))
                                                                       then ((show (coordX))++" "++(show (coordY + 1))++" "++"N")
                                                                       else if (orient == "E") && (coordX < (length (head txt) - 1)) && (ord (posInicialTab) == ord (posFinalTab)) || (orient == "E") && (coordX < (length (head txt) - 1)) && (ord (toLower posInicialTab) == ord (toLower posFinalTab))
                                                                            then ((show (coordX + 1))++" "++(show (coordY))++" "++"E")
                                                                            else if (orient == "S") && (coordY > 0) && (ord (posInicialTab) == ord (posFinalTab)) || (orient == "S") && (coordY > 0) && (ord (toLower posInicialTab) == ord (toLower posFinalTab))
                                                                                 then ((show (coordX))++" "++(show (coordY - 1))++" "++"S")
                                                                                 else "ERRO"


{- | A função 'validaSaltar' avalia se é ou não possível efetuar o comando "Saltar" em qualquer caso atribuído, para isso, certas condições deverão ser seguidas:

*O robot ao saltar não pode sair fora do tabuleiro em causa;

*Sabendo que o tabuleiro de jogo é constituído por letras, maiúsculas e minúsculas, o robot só salta de uma posição para outra se a letra da posição de onde ele parte for
inferior em apenas uma letra (p.e. de um 'a' para um 'b') em relação à posição onde ele cairá ou se a letra da posição de onde ele parte for superior, não importando quanto 
superior é, do que a letra para a posição onde ele cairá, p.e. o robot pode saltar de um 'x' para um 'a' independentemente de ser maiúscula ou minúscula.
-}


validaSaltar :: String -> Int -> Int -> Char -> Char -> [String] -> String
validaSaltar orient coordX coordY posInicialTab posFinalTab txt = if ((orient == "O") && (coordX > 0) && (ord (posInicialTab) + 1 == ord (posFinalTab) || ord (posFinalTab) < ord (posInicialTab))) || ((orient == "O") && (coordX > 0) && (ord (toLower (posInicialTab)) + 1 == ord (toLower (posFinalTab)) || ord (toLower (posFinalTab)) < ord (toLower (posInicialTab))))
                                                                  then ((show (coordX - 1))++" "++(show (coordY))++" "++"O")
                                                                  else if ((orient == "N") && (coordY < (length txt - 2 - 1)) && (ord (posInicialTab) + 1 == ord (posFinalTab) || ord (posFinalTab) < ord (posInicialTab))) || ((orient == "N") && (coordY < (length txt - 2 - 1)) && (ord (toLower (posInicialTab)) + 1 == ord (toLower (posFinalTab)) || ord (toLower (posFinalTab)) < ord (toLower (posInicialTab))))
                                                                       then ((show (coordX))++" "++(show (coordY + 1))++" "++"N")
                                                                       else if ((orient == "E") && (coordX < (length (head txt)) - 1) && (ord (posInicialTab) + 1 == ord (posFinalTab) || ord (posFinalTab) < ord (posInicialTab))) || ((orient == "E") && (coordX < (length (head txt)) - 1) && (ord (toLower (posInicialTab)) + 1 == ord (toLower (posFinalTab)) || ord (toLower (posFinalTab)) < ord (toLower (posInicialTab))))
                                                                            then ((show (coordX + 1))++" "++(show (coordY))++" "++"E")
                                                                            else if ((orient == "S") && (coordY > 0) && (ord (posInicialTab) + 1 == ord (posFinalTab) || ord (posFinalTab) < ord (posInicialTab))) || ((orient == "S") && (coordY > 0) && (ord (toLower (posInicialTab)) + 1 == ord (toLower (posFinalTab)) || ord (toLower (posFinalTab)) < ord (toLower (posInicialTab))))
                                                                                 then ((show (coordX))++" "++(show (coordY - 1))++" "++"S")
                                                                                 else "ERRO"


{- | A função 'validaLuz' avalia se o comando 'L' é executável, ou seja, verifica se:

* O comando 'L' é executado num sítio onde definitivamente existe uma luz para acender, ou seja, uma letra maiúscula.
-}

validaLuz :: String -> Int -> Int -> Char -> [String] -> String
validaLuz orient coordX coordY posInicialTab txt = if (orient == "O") && posInicialTab >= 'A' && posInicialTab <= 'Z'
                                                   then ((show (coordX))++" "++(show (coordY))++" "++"O")
                                                   else if (orient == "N") && posInicialTab >= 'A' && posInicialTab <= 'Z'
                                                        then ((show (coordX))++" "++(show (coordY))++" "++"N")
                                                        else if (orient == "E") && posInicialTab >= 'A' && posInicialTab <= 'Z'
                                                             then ((show (coordX))++" "++(show (coordY))++" "++"E")
                                                             else if (orient == "S") && posInicialTab >= 'A' && posInicialTab <= 'Z'
                                                                  then ((show (coordX))++" "++(show (coordY))++" "++"S")
                                                                  else "ERRO"


{- | A função 'validaDireita' verifica para que orientação ficará virado o robot dependendo da orientação inicial.
-}


validaDireita :: String -> Int -> Int -> [String] -> String
validaDireita orient coordX coordY txt = if (orient == "O")
                                         then ((show (coordX))++" "++(show (coordY))++" "++"N")
                                         else if (orient == "N")
                                              then ((show (coordX))++" "++(show (coordY))++" "++"E")
                                              else if (orient == "E")
                                                   then ((show (coordX))++" "++(show (coordY))++" "++"S")
                                                   else if (orient == "S")
                                                        then ((show (coordX))++" "++(show (coordY))++" "++"O")
                                                        else "ERRO"


{- | A função 'validaEsquerda' verifica para que orientação ficará virado o robot dependendo da orientação inicial.
-}

validaEsquerda :: String -> Int -> Int -> [String] -> String
validaEsquerda orient coordX coordY txt = if (orient == "O")
                                         then ((show (coordX))++" "++(show (coordY))++" "++"S")
                                         else if (orient == "N")
                                              then ((show (coordX))++" "++(show (coordY))++" "++"O")
                                              else if (orient == "E")
                                                   then ((show (coordX))++" "++(show (coordY))++" "++"N")
                                                   else if (orient == "S")
                                                        then ((show (coordX))++" "++(show (coordY))++" "++"E")
                                                        else "ERRO"
