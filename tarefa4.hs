module Main where

import Data.Char

type Jogo = [String]
type Tab = [String]
type Pos = (Int, Int)
type Cmd = Char
type Orient = Char
type Lampada = (Int, Int)

{- | A função 'main' é a função predefinida na linguagem de programação Haskell como o ponto de entrada de um programa.
-}

main = do inp <- getContents
          putStr (tarefa4 (lines inp))

{- | A função 'tarefa4' será a nossa função principal cujo objetivo é, a partir de um tabuleiro e posição inicial e orientação dados, construir a linha
dos comandos de forma a que o Robot acenda todas as lâmpadas contidas no mapa, concluindo desta forma o jogo.
Esta função recebe uma lista de String's, que irão corresponder às linhas do tabuleiro e da posição inicial e orientação e produz uma String correspondente à
linha dos comandos necessária para concluir o jogo.
-}

tarefa4 :: [String] -> String
tarefa4 txt = msg
       where msg = ligaTodasLampadas tab (x,y) orient l
             tab = takeWhile (all isAlpha) txt
             linhaCoord = take 1 (drop (length tab) txt)
             x = read(head (words (head linhaCoord))) 
             y = read(head (tail (words (head linhaCoord)))) 
             posInicial = (x,y)
             orient = head (last (words (head linhaCoord)))
             l = listaLampadas tab 0 0

-- ** Secção de Funções Auxiliares
-- Funções usadas para definir a função principal.


{- | A função 'coordLampadas' permite-nos saber a lista de posições das lâmpadas presentes no tabuleiro.
Esta função recebe um Tabuleiro e dois inteiros que serão a base da nossa contagem para a nossa coordenada x e
para a nossa coordenada y. Devolve assim uma lista de Lampada, ou seja, uma lista de pares, contendo, neste caso,
as coordenadas x e y da nossa lâmpada.
-}


coordLampadas :: Tab -> Int -> Int -> [Lampada]
coordLampadas [] a b = []
coordLampadas ((x:y):t) a b | (x >= 'A') && (x <= 'Z') && (y == []) = (a, b) : (coordLampadas t 0 (b + 1))
                            | (x >= 'a') && (x <= 'z') && (y == []) = (coordLampadas t 0 (b + 1))
                            | (x >= 'A') && (x <= 'Z') = (a, b) : (coordLampadas (y : t) (a + 1) b)
                            | (x >= 'a') && (x <= 'z') = (coordLampadas (y : t) (a + 1) b)


{- | A função 'listaLampadas' irá receber um tabuleiro qualquer irá produzir uma lista de pares, mas desta vez
com a ordem que nos é mais funcional.
-}

listaLampadas :: Tab -> Int -> Int -> [Lampada]
listaLampadas tab x y = reverse (coordLampadas (reverse tab) x y)


{- | A função 'avancaV' é a função que dado um tabuleiro, uma posição de origem, uma orientação e uma posição final, ou seja,
a posição à qual queremos que o Robot chegue, iguala a nossa coordenada y (ordenada), ou seja, faz o Robot chegar a uma casa onde
apenas será necessário mover-se horizontalmente.
-}


avancaV :: Tab -> Pos -> Orient -> Lampada -> (Orient, [Cmd])
avancaV tab (x, y) orient (x1, y1) | (y1 == y) = (orient, [])
                                   | (aos (x,y) (x,y+1) == 'e') = ((mudaO orient) , snd (avancaV tab (x, y) orient (x1, y1)))
                                   | (aos (x,y) (x,y-1) == 'e') = ((mudaO orient) , snd (avancaV tab (x, y) orient (x1, y1)))
                                   | (y1 > y) && (orient == 'N') = (fst (move (x, y + 1) orient), (aos (x, y) (x, y + 1)) : snd (move (x, y + 1) orient)) 
                                   | (y1 > y) && (orient == 'E') = (fst (move (x, y) 'N'), 'E' : snd (move (x, y) 'N'))
                                   | (y1 > y) && (orient == 'S') = (fst (move (x, y) 'N'), "EE" ++ snd (move (x, y) 'N')) 
                                   | (y1 > y) && (orient == 'O') = (fst (move (x, y) 'N'), 'D' : snd (move (x, y) 'N')) 
                                   | (y1 > y) = ((mudaO orient) , (snd (avancaV tab (x, y) (mudaO orient) (x1, y1))))
                                   | (y1 < y) && (orient == 'N') = (fst (move (x, y) 'S'), "EE" ++ snd (move (x, y) 'S'))
                                   | (y1 < y) && (orient == 'E') = (fst (move (x, y) 'S'), 'D' : snd (move (x, y) 'S')) 
                                   | (y1 < y) && (orient == 'S') = (fst (move (x, y - 1) orient), (aos (x, y) (x, y - 1)) : snd (move (x, y - 1) orient)) 
                                   | (y1 < y) && (orient == 'O') = (fst (move (x, y) 'S'), 'E' : snd (move (x, y) 'S'))  
                                   | (y1 < y) = ((mudaO orient) , (snd (avancaV tab (x, y) (mudaO orient) (x1, y1)))) 
                            where aos (a, b) (c, d) = saltarOuAvanca tab (a, b) (c, d)
                                  move (a, b) ori = avancaV tab (a, b) ori (x1, y1)


{- | A função 'avancaH' é a função que dado um tabuleiro, uma posição de origem, uma orientação e uma posição final, ou seja,
a posição à qual queremos que o Robot chegue, iguala a nossa coordenada x (abcissa), ou seja, faz o Robot chegar a uma casa onde
apenas será necessário mover-se verticalmente.
-}


avancaH :: Tab ->  Pos -> Orient -> Lampada -> (Orient,[Cmd]) 
avancaH  tab (x, y) orient (x1, y1)  | (x1 == x) = (orient, [])
                                     | (aos (x,y) (x+1,y) == 'e') = ((mudaO orient) , snd (avancaH tab (x, y) orient (x1, y1)))
                                     | (aos (x,y) (x-1,y) == 'e') = ((mudaO orient) , snd (avancaH tab (x, y) orient (x1, y1)))
                                     | (x1 > x) && (orient == 'N') = (fst (move (x, y) 'E'), 'D':(snd (move (x, y) 'E')))
                                     | (x1 > x) && (orient == 'E') = (fst (move (x + 1, y) orient), (aos (x, y) (x + 1, y)):(snd (move (x + 1, y) orient)))
                                     | (x1 > x) && (orient == 'S') = (fst (move (x, y) 'E'), 'E':(snd (move (x, y) 'E')))
                                     | (x1 > x) && (orient == 'O') = (fst (move (x, y) 'E'), "EE"++(snd (move (x, y) 'E')))
                                     | (x1 > x) = ((mudaO orient) , (snd (avancaH tab (x, y) (mudaO orient) (x1, y1))))
                                     | (x1 < x) && (orient == 'N') = (fst (move (x, y) 'O'), 'E':(snd (move (x, y) 'O')))
                                     | (x1 < x) && (orient == 'E') = (fst (move (x, y) 'O'), "EE"++(snd (move (x, y) 'O')))
                                     | (x1 < x) && (orient == 'S') = (fst (move (x, y) 'O'), 'D':(snd (move (x, y) 'O')))
                                     | (x1 < x) && (orient == 'O') = (fst (move (x - 1, y) orient), (aos (x, y) (x - 1, y)):(snd (move (x - 1, y) orient)))
                                     | (x1 < x) = ((mudaO orient) , (snd (avancaH tab (x, y) (mudaO orient) (x1, y1))))
                            where aos (a, b) (c, d) = saltarOuAvanca tab (a, b) (c, d)
                                  move (a, b) ori = avancaH tab (a, b) ori (x1, y1)


{- | A função 'mudaO' é a função que nos ajuda a mudar a orientação do Robot quando este encontra um obstáculo e não consegue avançar.
-}


mudaO :: Orient -> Orient
mudaO orient | (orient == 'N') = 'E'
             | (orient == 'E') = 'S'
             | (orient == 'S') = 'O'
             | (orient == 'O') = 'N'


{- | A função 'ligalampada' é a função que dado um tabuleiro, uma posição inicial e orientação e uma Lampada, faz com que o Robot
se dirija a estas coordenadas desta lâmpada e a ligue.
-}


ligalampada :: Tab -> Pos -> Orient -> Lampada -> (Orient,[Cmd])
ligalampada tab (x, y) orient (x1, y1)  = let sndOri = fst (avancaH tab (x, y) orient (x1, y1))
                                              lstOri = fst (avancaV tab (x1, y) sndOri (x1, y1))
                                          in (lstOri, (snd (avancaH tab (x, y) orient (x1, y1))) ++ (snd (avancaV tab (x1, y) sndOri (x1, y1))) ++ "L")


{- | A função 'saltarOuAvanca' permite-nos dar ao Robot uma decisão quando quer atingir a próxima casa do tabuleiro, sendo isto,
dependendo de algumas condições, o Robot saberá se deve avançar ou saltar.
-}


saltarOuAvanca :: Tab -> Pos -> Pos ->  Char
saltarOuAvanca tab (x, y) (x1, y1)  | (ord (toUpper (letraPos tab (x, y))) == ord (toUpper (letraPos tab (x1, y1))) - 1) = 'S'
                                    | ord (toUpper (letraPos tab (x, y))) == ord (toUpper (letraPos tab (x1, y1))) = 'A'
                                    | ord (toUpper (letraPos tab (x, y))) > ord (toUpper (letraPos tab (x1, y1))) = 'S'
                                    | otherwise = 'e'
                                     

{- | A função 'letraPos' é a função que dado um tabuleiro e uma posição vai ao tabuleiro buscar o caracter da casa respetiva a esta posição.
-}
           

letraPos :: Tab -> Pos -> Char
letraPos tab (x, y) = (head (drop y (reverse tab))) !! x


{- | A função 'ligaTodasLampadas' é a função que vai aplicar recursivamente a função 'ligalampada', fazendo com que, desta forma, todas
as lâmpadas contidas no tabuleiro sejam ligadas. 
-}                         

ligaTodasLampadas :: Tab -> Pos -> Orient -> [Lampada] -> [Cmd]
ligaTodasLampadas _ _ _ [] = []
ligaTodasLampadas tab (x,y) orient ((x1,y1):t) = (snd (ligalampada tab (x,y) orient (x1,y1)) ++ (ligaTodasLampadas tab (x1,y1) (fst (ligalampada tab (x,y) orient (x1,y1))) t ))
