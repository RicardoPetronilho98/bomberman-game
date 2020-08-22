module Main where

import Data.Char (isDigit)
import System.Environment
import Text.Read
import Data.Maybe
import Data.List
import Control.Monad

mapa8 = ["###############","#     ??      #","# # #?# #?# # #","# ? ?     ????#","# # # #?# #?#?#","#??   ?  ?    #","# # # # # # # #","#? ? ?  ?  ?? #","#?# # #?# # #?#","#   ????  ? ? #","# #?# # #?# # #","#?? ? ?? ?    #","# # #?# # #?# #","#      ? ? ?  #","###############","+ 1 7","+ 9 8","+ 6 11","! 3 4","! 11 7","! 12 7","! 13 8","* 1 1 3 2 1","* 3 3 1 3 1","* 10 7 1 3 3","* 4 13 0 1 7","* 5 13 2 2 1","0 1 2 +","1 2 9 +!!","2 4 13 !","3 3 8 !"]


main :: IO ()
main = do
    a <- getArgs
    let ticks = readMaybe (a !! 0)
    w <- getContents
    if isJust ticks
        then putStr $ unlines $ avanca (lines w) (fromJust ticks)
        else putStrLn "Parâmetros inválidos"

--------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------
avanca :: Mapa_Original -> Tempo -> Mapa_Original
avanca mapa t 
 | t <= (dim_Mapa mapa -2)^2 = reverte_Estado_de_jogo (destapa_tijolo_atingidos x, elimina_Power_Ups x, atualiza_Bombas x (raio_acao_de_todas_as_bombas x), atualiza_Jogadores x) 
 | otherwise = reverte_Estado_de_jogo (destapa_tijolo_atingidos e_j, elimina_Power_Ups e_j, atualiza_Bombas e_j (raio_acao_de_todas_as_bombas e_j), atualiza_Jogadores e_j) 
   where e_j = estado_de_jogo mapa
         x = retirar_PU_ou_B_ou_J (estado_de_jogo (coloca_pedra mapa t)) (qual_a_coord (get_matrix (dim_Mapa mapa-2)) (t_decc mapa t))
--------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------

reverte_Mapa :: Estado_de_jogo -> [String]
reverte_Mapa ([],_,_,_) = []
reverte_Mapa ((h:t),b,c,d) = [reverte_linha h] ++ reverte_Mapa (t,b,c,d) where
    reverte_linha [] = []
    reverte_linha (x:xs) = aux_l x ++ reverte_linha xs 
    aux_l Vazio = " "
    aux_l Tijolo = "?"
    aux_l Pedra = "#"

reverte_Power_Ups :: Estado_de_jogo -> [String]
reverte_Power_Ups (_,[],_,_) = []
reverte_Power_Ups (a,(x:xs),c,d) = aux_PU x ++ reverte_Power_Ups (a,xs,c,d) where
    aux_PU (PU_Bomba (c,l)) = ["+ " ++ show c ++ " " ++ show l]
    aux_PU (PU_Flame (c,l)) = ["! " ++ show c ++ " " ++ show l]

reverte_Bombas :: Estado_de_jogo -> [String]
reverte_Bombas (_,_,[],_) = []
reverte_Bombas (a,b,(x:xs),d) = aux_B x ++ reverte_Bombas (a,b,xs,d) where
    aux_B (Bomba_P (a,b,c,d,e)) = ["* " ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d ++ " " ++ show e]

reverte_jogadores :: Estado_de_jogo -> [String]
reverte_jogadores (_,_,_,[]) = []
reverte_jogadores (a,b,c,(x:xs)) = aux_J x ++ reverte_jogadores (a,b,c,xs) where
    aux_J (J a (c,l) pu_s) | pu_s == "" = [show a ++ " " ++ show c ++ " " ++ show l]
                           | otherwise = [show a ++ " " ++ show c ++ " " ++ show l ++ " " ++ pu_s]


reverte_Estado_de_jogo :: Estado_de_jogo -> Mapa_Original
reverte_Estado_de_jogo mapa = reverte_Mapa mapa ++ reverte_Power_Ups mapa ++ reverte_Bombas mapa ++ reverte_jogadores mapa  

--------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------
--ESTRUTURAÇÃO/ SEPARAÇÃO/ ORGANIZAÇÃO DO MAPA:

data Celulas = Pedra | Tijolo | Vazio deriving (Show,Eq)
data Power_Up = PU_Bomba Coordenada | PU_Flame Coordenada deriving (Show,Eq)
data Estado_da_Bomba = Bomba_P Quintuplo deriving (Show,Eq)
data Jogador = J Int Coordenada PUs deriving (Show,Eq)
--data Dir = 

type Tempo = Int
type Dimensao_do_Mapa = Int
type Numero_blocos_meio = Int
type Mapa_Original = [String]
type Mapa = [[Celulas]]
type Power_Ups = [Power_Up]
type Estado_do_Power_Up = String
type Coordenada = (Int,Int)
type Quintuplo = (Int,Int,Int,Int,Int)
type Jogadores = [Jogador]
type PUs = String
type Estado_de_jogo = (Mapa, Power_Ups, [Estado_da_Bomba], Jogadores)
type Raio_da_bomba = Int
type Lista_de_Coordenadas = [(Int,Int)]
type Raio_Coordenadas = [(Int,Int)]
type Mapa_Auxiliar = [[Int]]  
 

converte_mapa :: Mapa_Original -> Mapa
converte_mapa [] = []
converte_mapa (h:t) = [aux_converte h] ++ converte_mapa t where
  aux_converte [] = []
  aux_converte (x:xs) | x == ' ' = Vazio : aux_converte xs
                      | x == '?' = Tijolo : aux_converte xs
                      | x == '#' = Pedra : aux_converte xs

converte_Power_Ups :: [String] -> Power_Ups
converte_Power_Ups [] = []
converte_Power_Ups (x:xs) | head x == '+' = [PU_Bomba (aux (words x))] ++ converte_Power_Ups xs
                          | head x == '!' = [PU_Flame (aux (words x))] ++ converte_Power_Ups xs where
  aux [a,b,c] = (read b,read c)

converte_Bombas :: [String] -> [Estado_da_Bomba]
converte_Bombas [] = []
converte_Bombas (x:xs) = [Bomba_P (aux (words x))] ++ converte_Bombas xs where
  aux [a,b,c,d,e,f] = (read b,read c,read d,read e,read f)

converte_jogadores :: [String] -> Jogadores
converte_jogadores [] = []
converte_jogadores (x:xs) = [aux (words x)] ++ converte_jogadores xs where
  aux (a:b:c:d) | d == [] = J (read a) (read b,read c) ""
                | otherwise = J (read a) (read b,read c) (unwords d)
  

splitMapa :: [String] -> ([String],[String],[String],[String])
splitMapa xs = (tab,pus,bombas,players)
    where
    (tab,xs1) = span (\l -> head l == '#') xs
    (pus,xs2) = span (\l -> head l == '+' || head l == '!') xs1
    (bombas,players) = span (\l -> head l == '*') xs2

------------------------------------------------------------------------------------------------------------------------------------
estado_de_jogo :: Mapa_Original -> Estado_de_jogo
estado_de_jogo l = (converte_mapa a, converte_Power_Ups b, converte_Bombas c, converte_jogadores d) where
  (a,b,c,d) = splitMapa l
--------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Funções relativas ás bombas:

--recebe o estado de todos os Power Ups no mapa e indica as suas coordenadas numa lista:
converter_P_Ups_em_Coords :: Power_Ups -> Lista_de_Coordenadas
converter_P_Ups_em_Coords [] = []
converter_P_Ups_em_Coords (x:xs) = [pu_coords x] ++ converter_P_Ups_em_Coords xs 

--determina as coordenadas de uma bomba (a sua localização no mapa):
bomba_coords :: Estado_da_Bomba -> Coordenada
bomba_coords (Bomba_P (a,b,_,_,_)) = (a,b)  
--ex: Bomba (3,5,2,1,10) = (3,5)

--determina as coordenadas das chamas na esplosão da bomba consoante o mapa dado (se a chama atingir um '#' é bloqueada, se atingir um '?' faz esplodir esse elemento mas depois é bloqueada, noutro caso qualquer esplode esse elemento):
raio_acao_bomba :: Estado_de_jogo -> Estado_da_Bomba -> Raio_Coordenadas
raio_acao_bomba (a,b,_,_) (Bomba_P (c,l,_,raio,_)) = cima a (c,l-1) raio e ++ baixo a (c,l+1) raio e ++ direita a (c+1,l) raio e ++ esquerda a (c-1,l) raio e ++ [(c,l)] where 
    e = converter_P_Ups_em_Coords b
    cima _ _ 0 _ = []
    cima mapa (c,l) n powerUps | qual_o_caracter mapa (c,l) == Pedra = []
                               | qual_o_caracter mapa (c,l) == Tijolo || elem (c,l) powerUps = [(c,l)]
                               | otherwise = [(c,l)] ++ cima mapa (c,l-1) (n-1) powerUps
    baixo _ _ 0 _ = []
    baixo mapa (c,l) n powerUps | qual_o_caracter mapa (c,l) == Pedra = []
                                | qual_o_caracter mapa (c,l) == Tijolo || elem (c,l) powerUps = [(c,l)]
                                | otherwise = [(c,l)] ++ baixo mapa (c,l+1) (n-1) powerUps
    direita _ _ 0 _ = []
    direita mapa (c,l) n powerUps | qual_o_caracter mapa (c,l) == Pedra = []
                                  | qual_o_caracter mapa (c,l) == Tijolo || elem (c,l) powerUps = [(c,l)]
                                  | otherwise = [(c,l)] ++ direita mapa (c+1,l) (n-1) powerUps
    esquerda _ _ 0 _ = []
    esquerda mapa (c,l) n powerUps | qual_o_caracter mapa (c,l) == Pedra = []
                                   | qual_o_caracter mapa (c,l) == Tijolo || elem (c,l) powerUps = [(c,l)]
                                   | otherwise = [(c,l)] ++ esquerda mapa (c-1,l) (n-1) powerUps   
--raio_acao_bomba mapa3 (Bomba (3,5,2,1,10)) [(5,2),(3,3),(5,5)] = [(3,4),(3,6),(4,5),(2,5),(3,5)]

raio_acao_de_todas_as_bombas :: Estado_de_jogo -> Raio_Coordenadas
raio_acao_de_todas_as_bombas mapa@(_,_,c,_) = concat $ aux1 mapa (reduz_Tempo_Bomba (quais_vao_explodir c)) where
    aux1 _ [] = []
    aux1 mapa (x:xs) = raio_acao_bomba mapa x : aux1 mapa xs

reduz_Tempo_Bomba :: [Estado_da_Bomba] -> [Estado_da_Bomba]
reduz_Tempo_Bomba [] = [] 
reduz_Tempo_Bomba ((Bomba_P (a,b,c,d,tempo)):xs) = (Bomba_P (a,b,c,d,tempo-1)) : reduz_Tempo_Bomba xs


--funcao que verifica se uma bomba vai explodir, em caso afirmativo retorna as bombas que vao esplodir:
quais_vao_explodir :: [Estado_da_Bomba] -> [Estado_da_Bomba]
quais_vao_explodir [] = [] 
quais_vao_explodir (bomba@(Bomba_P (_,_,_,_,tempo)):xs) | tempo == 1 = bomba : quais_vao_explodir xs
                                                        | otherwise = quais_vao_explodir xs

--Retorna os Estados das Bombas usados no mapa Atualizado (é o que vai ser usado na função avançca):
--testa se o estado de bomba em causa está dentro do raio de esplosão de outra bomba, em caso afirmativo reduz o temporizador dessa bomba passa para 1, de forma a forçar a sua explosão no próximo instante de tempo.
--em caso negativo retorna o mesmo estado de bomba:
atualiza_Bombas :: Estado_de_jogo -> Raio_Coordenadas -> [Estado_da_Bomba]
atualiza_Bombas (_,_,[],_) _ = []
atualiza_Bombas (a,b,e_b@(x:xs),d) r_lista = aux_retirar (quais_vao_explodir e_b) (aux_atualiza_Bombas x r_lista : atualiza_Bombas (a,b,xs,d) r_lista) where
   aux_atualiza_Bombas b r_lista | elem (bomba_coords b) r_lista == False = head (reduz_Tempo_Bomba [b])
                                 | otherwise = aux_2 b 
   aux_2 (Bomba_P (a,b,c,d,e)) = Bomba_P (a,b,c,d,1)
   aux_retirar [] l = l
   aux_retirar (x:xs) l = aux_retirar xs (delete x l) 
--atualiza_Bombas mapa6 = [Bomba_P (7,7,0,7,1),Bomba_P (2,1,0,3,6)]

--------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Funções relativas aos Power_Ups:

--determina as coordenadas de um Power_Up (a sua localização no mapa):
pu_coords :: Power_Up -> Coordenada
pu_coords (PU_Bomba coord) = coord
pu_coords (PU_Flame coord) = coord

--lista de coordenadas PowerUps que vão ser destapados:
deteta_P_Ups__destapados_atingidos :: Estado_de_jogo -> Raio_Coordenadas -> Power_Ups
deteta_P_Ups__destapados_atingidos (a,[],c,d) _ = []
deteta_P_Ups__destapados_atingidos mapa@(a,(x:xs),c,d) raio_coords
      | elem (pu_coords x) raio_coords && qual_o_caracter a (pu_coords x) == Vazio = x : deteta_P_Ups__destapados_atingidos (a,xs,c,d) raio_coords
      | otherwise = deteta_P_Ups__destapados_atingidos (a,xs,c,d) raio_coords

deteta_Tijolos_atingidos :: Estado_de_jogo -> Lista_de_Coordenadas -> Lista_de_Coordenadas
deteta_Tijolos_atingidos _ [] = []
deteta_Tijolos_atingidos mapa (h:t)
      | elem h (raio_acao_de_todas_as_bombas mapa) = [h] ++ deteta_Tijolos_atingidos mapa t
      | otherwise = deteta_Tijolos_atingidos mapa t


--recebe o mapa e uma valor e indica qual a coordenada nessa posição:
coord_dos_tijolos :: Mapa -> Int -> Lista_de_Coordenadas 
coord_dos_tijolos [] _ = []
coord_dos_tijolos (h:t) l = c_aux h Tijolo 0 l where 
    c_aux [] _ _ _ = coord_dos_tijolos t (l+1)
    c_aux (x:xs) n c l | x == n = [(c,l)] ++ c_aux xs n (c+1) l 
                       | otherwise = c_aux xs n (c+1) l

--deteta_P_Ups_atingidos [(5,2),(3,3),(5,5)] [(3,4),(3,3),(3,6),(4,5),(5,5),(2,5),(3,5)] = [(3,3),(5,5)]
--recebe o (just_mapa) mais a coordenada a ser destapada e retorna o mapa atualizado:
--usa a mecanica semelhante á função (qual_o_caracter): 
destapa_tijolo :: Mapa -> Coordenada -> Mapa
destapa_tijolo mapa (c,l) = l_aux mapa (c,l) 0 where 
    l_aux [] _ _ = []
    l_aux (x:xs) (c,l) l1 | l == l1 = [c_aux x c 0] ++ l_aux xs (c,l) (l1+1)
                          | otherwise = [x] ++ l_aux xs (c,l) (l1+1)    
    c_aux [] _ _ = []
    c_aux (x:xs) c c1 | c == c1 = [Vazio] ++ c_aux xs c (c1+1)
                      | otherwise = [x] ++ c_aux xs c (c1+1)
--destapa_tijolo mapa3 (3,3)

elimina_Power_Ups :: Estado_de_jogo -> Power_Ups 
elimina_Power_Ups mapa = aux mapa $ deteta_P_Ups__destapados_atingidos mapa (raio_acao_de_todas_as_bombas mapa) where
     aux (_,b,_,_) [] = b
     aux (a,b,c,d) (x:xs) = aux (a,(delete x b),c,d) xs
--elimina_Power_Up (PU_Flame (5,5)) mapa2

destapa_tijolo_atingidos :: Estado_de_jogo -> Mapa
destapa_tijolo_atingidos mapa@(a,b,c,d) = aux mapa (deteta_Tijolos_atingidos mapa (coord_dos_tijolos a 0)) where
    aux (a,_,_,_) [] = a
    aux mapa@(a,b,c,d) (x:xs) = aux ((destapa_tijolo a x),b,c,d) xs  

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Funções relativas aos jogadores:

jogador_coords :: Jogador -> Coordenada
jogador_coords (J a b c) = b

atualiza_Jogadores :: Estado_de_jogo -> Jogadores 
atualiza_Jogadores mapa@(a,b,c,d) = aux mapa (length d)
aux (a,b,c,d) 0 = d
aux mapa@(a,b,c,(x:xs)) n | elem (jogador_coords x) (raio_acao_de_todas_as_bombas mapa) = aux (a,b,c,(xs)) (n-1)
                          | otherwise = [x] ++ aux (a,b,c,(xs)) (n-1)                           

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Funções auxiliares usadas ao longo de toda a Tarefa:

--recebe o mapa e uma coordenada e indica qual o caracter nessa posição:
qual_o_caracter :: Mapa -> Coordenada -> Celulas 
qual_o_caracter mapa (c,l) = l_aux mapa (c,l) 0 where 
    l_aux (x:xs) (c,l) l1 | l == l1 = c_aux x c 0
                          | otherwise = l_aux xs (c,l) (l1+1) 
    c_aux (x:xs) c c1 | c == c1 = x 
                      | otherwise = c_aux xs c (c1+1)
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Funções usadas ao longo para a espiral:

dim_Mapa :: Mapa_Original -> Dimensao_do_Mapa 
dim_Mapa mapa = length (head mapa)

t_decc :: Mapa_Original -> Tempo -> Int
t_decc mapa t = (dim_Mapa mapa-2)^2 - (t-1) 

--------------------------------------------------------------------------------------------------------------------
get_list n = nub $ aux1 n ++ aux2 n 1 ++ aux3 [(c,l) | c <- [1..n],  l <- [1..n], c == l] (div n 2 +1) ++ [(n,n)]

aux1 n = aux [(c,l) | c <- [1..n],  l <- [1..n], c == l] n
       where aux ((c,l):xs) n | c == n = []
                              | c <= (div n 2 + 1) = aux xs n
                              | otherwise = (c,l) : aux xs n

aux2 0 _ = []
aux2 s x = [(s,x)] ++ aux2 (s-1) (x+1) 

aux3 _ 0 = []
aux3 ((c,l):xs) x = [(c-1,l)] ++ aux3 xs (x-1)



--v começa em 1:
--ci começa em (dimensão-2): 
--li começa em 1:
--w começa em 3:

aux4 lista (c,l) (cv,lv) v w | (cv,lv) == (c,l) = v 
                            | elem (cv,lv) lista && w == 3 = aux4 lista (c,l) (cv,lv+1) (v+1) 0 -- canto superior direito (CHECKED!)
                            | elem (cv,lv) lista && w == 0 = aux4 lista (c,l) (cv-1,lv) (v+1) 1 -- canto inferior direito (CHECKED!)
                            | elem (cv,lv) lista && w == 1 = aux4 lista (c,l) (cv,lv-1) (v+1) 2 -- canto inferior esquerdo
                            | elem (cv,lv) lista && w == 2 = aux4 lista (c,l) (cv+1,lv) (v+1) 3 
                            | w == 0 = aux4 lista (c,l) (cv,lv+1) (v+1) 0 -- ir para baixo  
                            | w == 1 = aux4 lista (c,l) (cv-1,lv) (v+1) 1 -- ir para a esquerda
                            | w == 2 = aux4 lista (c,l) (cv,lv-1) (v+1) 2 -- ir para cima
                            | w == 3 = aux4 lista (c,l) (cv+1,lv) (v+1) 3 -- ir para a direita



get_matrix s = split' s (aux_Final (1,1) s (get_list s) ++ [2*s-1])
 where aux_Final (c,l) s lista | (c,l) == (s,s) = []
                               | c == s+1 = aux_Final (1,l+1) s lista 
                               | otherwise = aux4 lista (c,l) (1,1) 1 3 : aux_Final (c+1,l) s lista 


split' :: Int -> [Int] -> [[Int]]
split' _ [] = []
split' s lista = [take s lista] ++ split' s (drop s lista)



---------------------------------------------------------------------------------------------------------

--recebe o mapa e uma valor e indica qual a coordenada nessa posição:
qual_a_coord :: Mapa_Auxiliar -> Int -> Coordenada 
qual_a_coord mapa n = l_aux mapa n 1 where 
    l_aux (h:t) n l | elem n h = c_aux h n 1 l
                    | otherwise = l_aux t n (l+1)
    c_aux (x:xs) n c l | x == n = (c,l) 
                       | otherwise = c_aux xs n (c+1) l

coloca_pedra :: Mapa_Original -> Tempo -> Mapa_Original
coloca_pedra mapa t = l_aux mapa (qual_a_coord (get_matrix (dim_Mapa mapa-2)) (t_decc mapa t)) 0 where 
    l_aux [] _ _ = []
    l_aux (x:xs) (c,l) l1 | l == l1 = [c_aux x c 0] ++ l_aux xs (c,l) (l1+1)
                          | otherwise = [x] ++ l_aux xs (c,l) (l1+1)    
    c_aux [] _ _ = []
    c_aux (x:xs) c c1 | c == c1 = "#" ++ c_aux xs c (c1+1)
                      | otherwise = [x] ++ c_aux xs c (c1+1)
--coloca_pedra mapa8 (1,1)

jogadore_s_coords :: Jogadores -> Lista_de_Coordenadas
jogadore_s_coords [] = []
jogadore_s_coords (x:xs) = [jogador_coords x] ++ jogadore_s_coords xs

elimina_jogador :: Coordenada -> Estado_de_jogo -> Jogadores
elimina_jogador coord (a,b,c,(x:xs)) | jogador_coords x == coord = xs
                                     | otherwise = [x] ++ elimina_jogador coord (a,b,c,xs)

bomba_s_coords :: [Estado_da_Bomba] -> Lista_de_Coordenadas
bomba_s_coords [] = []
bomba_s_coords (x:xs) = [bomba_coords x] ++ bomba_s_coords xs

elimina_Bomba :: Coordenada -> Estado_de_jogo -> [Estado_da_Bomba]
elimina_Bomba coord (a,b,(x:xs),d) | bomba_coords x == coord = xs 
                                   | otherwise = [x] ++ elimina_Bomba coord (a,b,xs,d)

retirar_PU_ou_B_ou_J :: Estado_de_jogo -> Coordenada -> Estado_de_jogo
retirar_PU_ou_B_ou_J mapa@(a,b,c,d) coord 
      | elem (PU_Flame coord) b = (a,delete (PU_Flame coord) b,c,d)
      | elem (PU_Bomba coord) b = (a,delete (PU_Bomba coord) b,c,d)
      | elem coord (bomba_s_coords c) = (a,b,elimina_Bomba coord mapa,d)
      | elem coord (jogadore_s_coords d) = (a,b,c,elimina_jogador coord mapa)
      | otherwise = mapa
