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
          putStr (outStr (tarefa1 (lines inp)))


{- | A função 'tarefa1' irá permitirnos verificar se um certo tabuleiro, posição inicial e orientação e lista de comandos para o jogo LightBot estão ou não bem contruídos.
Caso não esteja bem contruído irá então indicar a linha de código do respetivo teste onde se encontra esse erro.
Um mapa é então considerado válido se obeceder às seguintes condições:

* Tem pelo menos três linhas (uma para o tabuleiro, outra que será a posição inicial e orientação e outra que deve ser a linha dos comandos);

* Pode ser dividida em Tabuleiro, Linha Da Posição Inicial e Orientação e Linha dos Comandos, em que cada uma destas partes deverá obedecer a certas condições específicas
para serem válidas.

* Após a Linha dos Comandos não deverá existir mais nada.
-}


tarefa1 :: [String] -> [String]
tarefa1 [] = ["1"]
tarefa1 txt = [msg]
    where tab = takeWhile (all isAlpha) txt -- Vai buscar as linhas do tabuleiro ao input
          erroTab = validaTab tab 
          linhaCoord = drop (length tab) txt -- Vai buscar a linha das coordenadas e orientação ao input
          dimx = length (head txt)
          dimy = length tab
          erroLinhaCoord = coordValidas dimx dimy linhaCoord
          prog = drop (length tab + 1) txt -- Vai buscar a linha dos comandos ao input
          erroProg = progValido dimy prog
          msg = if length txt < 2
                then "1"
                else (if erroTab > 0
                      then show erroTab
                      else (if erroLinhaCoord > 0
                            then show erroLinhaCoord
                            else (if erroProg > 0
                                  then show erroProg
                                  else "OK")))


-- ** Secção de Funções Auxiliares
-- Funções usadas para definir a função principal.

{- | A função 'validaTab' irá verificar a primeira parte do mapa fornecido, neste caso o tabuleiro, que, para ser válido, deve seguir as seguintes condições:

* Todas as linhas pertencentes ao tabuleiro têm de possuir o mesmo tamanho;

* O tabuleiro só pode conter letras, sejam elas, minúsculas ou maiúsculas, mas sem caracteres especiais, p.e. 'á'.
-}


validaTab :: [String] -> Int
validaTab [] = 1
validaTab (h:t) = aux (length h) 1 (h:t)
    where aux tam n [] = 0
          aux tam n (y:ys) = if length y == tam && testaLetras y
                             then aux tam (n+1) ys
                             else n
          testaLetras [] = True
          testaLetras (x:xs) = if (x >= 'a' && x <= 'z' || x >= 'A' && x <= 'Z') && testaLetras xs
                               then True
                               else False


{- | A função 'coordValidas' verifica a segunda parte do mapa, a Linha da Posição Inicial e Orientação, que, mais uma vez, terá de obedecer a uma lista de regras, são elas:

* Esta linha deverá ser constituída por um 'Int' seguido de um espaço, novamente um 'Int', 
outro espaço e por fim a orientação (String);

* A orientação terá que ser uma das seguintes letras: "N", "S", "E", "O";

* As coordenadas deverão estar dentro do tabuleiro dado.
-}


coordValidas :: Int -> Int -> [String] -> Int
coordValidas dimx dimy [] = dimy + 1
coordValidas dimx dimy (l:_) = if testaCoord l 
                               then 0 
                               else dimy + 1
    where testaCoord :: String -> Bool
          testaCoord str = case words str of
                          [x, y, [o]] -> if isNum x && (read x) < dimx
                                            && isNum y && (read y) < dimy
                                            && (o == 'N' || o == 'S' || o == 'E' || o == 'O')
                                         then True
                                         else False
                          _ -> False
          isNum [] = True
          isNum (x:xs) = isDigit x && isNum xs


{- | A função 'progValido' verifica por fim a terceira parte do mapa, ou seja, a Linha dos Comandos. Tal como para as outras partes certas regras deverão ser obedecidas:

* A Linha dos Comandos deverá ser constituída apenas pelos caracteres 'A', 'S', 'D', 'E' ou 'L';

* Posteriormente a esta linha não poderá existir nenhuma outra.
-}


progValido :: Int -> [String] -> Int
progValido dimy [] = dimy + 2
progValido dimy (h:t) = if testaCmds h 
                        then (if t == []
                              then 0
                              else (dimy + 3)) 
                        else (dimy + 2)
    where testaCmds [] = True
          testaCmds (x:xs) = if (x == 'A' || x == 'D' || x == 'L' || x == 'S' || x == 'E')
                             then testaCmds xs
                             else False