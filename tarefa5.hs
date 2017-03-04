module Main where

import Data.List
import Data.Char

type Jogo = [String]
type Tab = [String]
type Cmd = Char
type Pos = (Int, Int)
type Lampada = (Int, Int)
type Orient = Char

{- | A função 'outStr' é uma função que dado o mapa em formato txt, ou seja, quando visto em forma de String, separado por '/n', corta por este mesmo separador em várias String's.
-}

outStr :: [String] -> String
outStr [] = "\n"
outStr t = unlines t

{- | A função 'main' é a função predefinida na linguagem de programação Haskell como o ponto de entrada de um programa.
-}

main = do inp <- getContents
          putStr (outStr (tarefa5 (lines inp)))

{- | A função 'tarefa5' é a nossa função principal que irá construir o código xhtml que nos permitirá visualizar em três 
dimensões o tabuleiro fornecido à tarefa, bem como a posição inicial e orientação do Robot.
Esta função recebe assim uma lista de String's, correspondentes às linhas do tabuleiro, à linha da posição inicial e orientação e
a linha dos comandos, produzindo, também, no output uma lista de String's que correspondem às linhas do nosso xhtml.
-}

tarefa5 :: Jogo -> [String]
tarefa5 txt = criaTab tab
          where tab = takeWhile (all isAlpha) txt
                linhaCoord = take 1 (drop (length tab) txt)
                coordX = read (head (words (head linhaCoord))) :: Int
                coordY = read (head (tail (words (head linhaCoord)))) :: Int
                posInicial = (coordX, coordY)
                orient = head (last (words (head linhaCoord)))
                criaTab tab = preFuncao ++ (converteLista (listaPosicoes tab 0 0)) ++ insereRobot txt ++ posFuncao
                preFuncao = ["<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"",
                              "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">",
                              "<html xmlns=\"http://www.w3.org/1999/xhtml\">",
                              "<head>",
                              "<meta http-equiv=\"X-UA-Compatible\" content=\"chrome=1\" />",
                              "<meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\" />",
                              "<title>LightBot</title>",
                              "<script type=\"text/javascript\" src=\"http://www.x3dom.org/release/x3dom.js\"></script>",
                              "<link rel=\"stylesheet\" type=\"text/css\" href=\"http://www.x3dom.org/release/x3dom.css\"/>",
                              "</head>",
                              "<body>",
                              "<h1>My LightBot Tab</h1>",
                              "<p class=\"case\">",
                              "<X3D xmlns=\"http://www.web3d.org/specifications/x3d-namespace\" id=\"boxes\"",
                              "showStat=\"false\" showLog=\"false\" x=\"0px\" y=\"0px\" width=\"600px\" height=\"600px\">",
                              "<Scene>",
                              "<Shape DEF=\"tile\">",
                              "<Appearance>",
                              "<Material diffuseColor='0 1.0 1.0'/>",
                              "</Appearance>",
                              "<Box size='.98 .98 .98'/>",
                              "</Shape>",
                              "<Shape DEF=\"tileL\">",
                              "<Appearance>",
                              "<Material diffuseColor='0 0 1.0'/>",
                              "</Appearance>",
                              "<Box size='.98 .98 .98'/>",
                              "</Shape>"]

                posFuncao = ["</Scene>",
                              "</X3D>",
                              "</p>",
                              "<p> &nbsp; </p>",
                              "</body>",
                              "</html>"]

                insereRobot txt = [ "<scene>",
                                    "<transform translation='0 " ++ show (aplica (toLower (letraPos tab posInicial)) * (fromIntegral (ord (toLower (letraPos tab posInicial)) - ord 'a' + 1))) ++ " 0'>",
                                    "<transform scale='0.2 0.2 0.2'>",
                                    "<transform translation='" ++ (show coordX) ++ " " ++ show ((fromIntegral (ord (toLower (letraPos tab posInicial)))) - ord 'a') ++ " " ++ show (-coordY) ++ "' rotation='0 0 1" ++ show (orientRobot orient) ++ "'>", 
                                    "<shape>",
                                    "<appearance>", 
                                    "<material diffuseColor='0.511 0.511 0.511'></material>",
                                    "</appearance>",
                                    "<box></box>",
                                    "</shape>",
                                    "<transform translation='0 1.5 0'>",
                                    "<shape>",
                                    "<appearance>",
                                    "<material diffuseColor='0 0 0'></material>",
                                    "</appearance>",
                                    "<sphere radius='0.5'></sphere>",
                                    "</shape>",
                                    "</transform>",
                                    "<transform translation='1 1 1'>", 
                                    "<shape>",
                                    "<appearance>",
                                    "<material diffuseColor='0.511 0.511 0.511'></material>", 
                                    "</appearance>",
                                    "<cylinder radius='0.3'/>", 
                                    "<cylinder></cylinder>",
                                    "</shape>",
                                    "<transform translation='-2 0 0'>",
                                    "<shape>",
                                    "<appearance>",
                                    "<material diffuseColor='0.511 0.511 0.511'></material>",
                                    "</appearance>",
                                    "<cylinder radius='0.3'/>",
                                    "<cylinder></cylinder>",
                                    "</shape>",
                                    "<transform translation='1 -2 -1'>",
                                    "<shape>",
                                    "<appearance>",
                                    "<material diffuseColor='0.311 0.311 0.311'></material>",
                                    "</appearance>",
                                    "<sphere radius='0.5'></sphere>",
                                    "<sphere></sphere>",
                                    "</transform>",
                                    "<transform translation='0.5 -3.5 -1'>",
                                    "<shape>",
                                    "<appearance>",
                                    "<material diffuseColor='0.511 0.511 0.511'></material>",
                                    "</appearance>",
                                    "<cylinder radius='0.3'/>",
                                    "<cylinder></cylinder>",
                                    "</shape>",
                                    "</transform>",
                                    "<transform translation='1.5 -3.5 -1'>",
                                    "<shape>",
                                    "<appearance>",
                                    "<material diffuseColor='0.511 0.511 0.511'></material>",
                                    "</appearance>",
                                    "<cylinder radius='0.3'/>",
                                    "<cylinder></cylinder>",
                                    "</shape>",
                                    "</transform>"]

                orientRobot orient | orient == 'N' = 1.5
                                   | orient == 'E' = 3
                                   | orient == 'S' = 0
                                   | orient == 'O' = 4.5

                aplica :: Char -> Float
                aplica a | a == 'a' = 1.15
                         | a == 'b' = 1.01
                         | a == 'c' = 0.92
                         | a == 'd' = 0.87
                         | a == 'e' = 0.85
                         | a == 'f' = 0.83
                         | a == 'g' = 0.823
                         | a == 'h' = 0.82
                         | a == 'i' = 0.8182
                         | a == 'j' = 0.8176
                         | a == 'k' = 0.8173
                         | a == 'l' = 0.81713
                         | a == 'm' = 0.817121
                         | a == 'n' = 0.817116
                         | a == 'o' = 0.817113
                         | a == 'p' = 0.817111
                         | a == 'q' = 0.817110
                         | a == 'r' = 0.817109
                         | a == 's' = 0.8171095
                         | a == 't' = 0.8171091
                         | a == 'u' = 0.8171087
                         | a == 'v' = 0.8171084
                         | a == 'w' = 0.8171082
                         | a == 'x' = 0.817108
                         | a == 'y' = 0.8171079
                         | a == 'z' = 0.81710785


-- ** Secção de Funções Auxiliares
-- Funções usadas para definir a função principal.


{- | A função 'posicao' permite-nos saber a lista de posições de todas as casas do tabuleiro.
Esta função recebe um Tabuleiro e dois inteiros que serão a base da nossa contagem para a nossa coordenada x e
para a nossa coordenada y. Devolve assim uma lista de tuplos, contendo, neste caso, as coordenadas x e y da nossa
casa, a cota desta mesma casa (sendo o caracter 'a' ou 'A' o nivel básico, correspondendo então ao valor 0,
'b' ou 'B' corresponde a 1 e assim sucessivamente) e o caracter dessa mesma casa, ou seja 'A', 'B', etc.
-}


posicao :: Tab -> Int -> Int -> [(Int, Int, Int, Char)]
posicao [] a b = []
posicao ((x:y):t) a b | (x == 'a') && (y == []) = (a, b, (ord (toLower x) - ord 'a'), x) : (posicao t (a + 1) 0)
                      | (x /= 'a') && (y == []) = (a, b, (ord (toLower x) - ord 'a'), x) : (posicao t (a + 1) 0)
                      | (x == 'a') = (a, b, (ord (toLower x) - ord 'a'), x) : (posicao (y:t) a (b + 1))
                      | (x /= 'a') = (a, b, (ord (toLower x) - ord 'a'), x) : (posicao (y:t) a (b + 1))


{- | A função 'listaPosicoes' irá receber um tabuleiro qualquer irá produzir uma lista de tuplos, mas desta vez
com a ordem que nos é mais funcional.
-}


listaPosicoes :: Tab -> Int -> Int -> [(Int, Int, Int, Char)]
listaPosicoes tab x y = (posicao (reverse tab) x y)


{- | A função 'verifica' recebe um Int e, tal como o nome da função indica, verifica se este inteiro é ou não
igual a 0, produzindo uma resposta Booleana.
-}


verifica :: Int -> Bool
verifica x = if x == 0
             then True
             else False


{- | A função 'converte' recebe um tuplo do mesmo tipo de um dos elementos da lista produzida pela função 'listaPosicoes'
e devolve uma String que depende de várias condições:

*O caracter não é maiúsculo, ou seja, aquela casa do tabuleiro não contém uma luz, e o terceiro inteiro do tuplo atende
às condições da função 'verifica';

*O caracter é igual a 'A', ou seja, naquela casa do tabuleiro existe uma lâmpada e o nível é o básico;

*O caracter é maiúsculo, mas diferente de 'A', ou seja, existe nesta casa do tabuleiro uma lâmpada e o nível não é básico;

*O caracter não atende às condições da função 'verifica'.
-}


converte :: (Int, Int, Int, Char) -> String
converte (x, y, z, a) = if (not (a >= 'A' && a <= 'Z')) && verifica z
                        then "<transform translation=\"" ++ (show x) ++ " " ++ (show z) ++ " " ++ (show y) ++ "\"> <shape use=\"tile\"/> </transform>"
                        else if a == 'A'
                             then "<transform translation=\"" ++ (show x) ++ " " ++ (show z) ++ " " ++ (show y) ++ "\"> <shape use=\"tileL\"/> </transform>"
                             else if a > 'A' && a <= 'Z'
                                  then converte (x, y, z - 1, 'a') ++ "<transform translation=\"" ++ (show x) ++ " " ++ (show z) ++ " " ++ (show y) ++ "\"> <shape use=\"tileL\"/> </transform>"
                                  else converte (x, y, z - 1, 'a') ++ "<transform translation=\"" ++ (show x) ++ " " ++ (show z) ++ " " ++ (show y) ++ "\"> <shape use=\"tile\"/> </transform>"


{- | A função 'converteLista' aplica recursivamente a função 'converte' a uma lista de tuplos, recebendo, como é o caso do
output da função 'listaPosicoes', a anteriormente falada lista de tuplos e devolvendo uma lista de String's.
-}


converteLista :: [(Int, Int, Int, Char)] -> [String]
converteLista [] = []
converteLista ((x, y, z, a):t) = converte (x, y, z, a) : converteLista t


{- | A função 'letraPos' permitir-nos-á obter o caracter de uma dada posição (par coordenado).
Esta função recebe então um Tabuleiro e uma Posição, que é um par coordenado e devolve o caracter correspondente
a essa posição no tabuleiro.
-}


letraPos :: Tab -> Pos -> Char
letraPos tab (x, y) = (head (drop y (reverse tab))) !! x