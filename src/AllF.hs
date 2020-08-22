module AllF where


import System.Random
import System.Environment
import Text.Read
import Data.Maybe
import Data.Char (isDigit)
import Data.List




------------- TAREFA 1 -------------------------------------------------------------------------------

--calcular o número de calhaus e espaços que vão ser preenchidos aleatóriamente ((n - 2)² - 12 - (nº de calhaus))
nBoulders :: Int -> Int
nBoulders n = (nlevel n) * (nlevel n) where 
  nlevel 5 = 1
  nlevel x = 1 + nlevel (x - 2)

nRandomBlocks :: Int -> Int
nRandomBlocks x = (x-2)^2 - 12 - nBoulders x

-------

--gerar uma lista aleatória de números correspondentes a tijolos, PUps e espaços vazios	
aleatorio :: Int -> Int -> [Int]
aleatorio size seed = take (nRandomBlocks size) $ randomRs (0,99) (mkStdGen seed)

--Correspondência entre números de 0 a 99 e o seu conteúdo(tijolos, bombs, flames)
--Os números que não figuram nestas funções vão ser espaços vazios
cmpBetween :: Int -> (Int, Int) -> Bool
cmpBetween x (a, b) | a > b = False
            | x == a = True
            | otherwise = cmpBetween x (a+1, b) 

isBomb :: Int -> Bool
isBomb x = cmpBetween x (0, 1)

isFlames :: Int -> Bool
isFlames x = cmpBetween x (2, 3)

isBrick :: Int -> Bool
isBrick x = cmpBetween x (4, 39)
-----

--Imprime o mapa numa String. recebe como argumento o tamanho do mapa (3x) e a lista de números (calculada com o randomize)
-- que vai conter os PUps e tijolos. Quando acaba de  percorrer todos os elementos da lista, continua acudindo à auxiliar.
desenha_Mapa :: Int -> Int -> Int -> [Int] -> String
desenha_Mapa 0 _ _ _ = []
desenha_Mapa line size x [] = mapa_aux  line size x 
desenha_Mapa line size x (h:t) | x == 1 = '#' : desenha_Mapa (line-1) size size (h:t)
             | line == 1 || line == size || x == size || (mod line 2 == 1 && mod x 2 == 1) = '#' : (desenha_Mapa line size (x-1) (h:t))
             | ((line == size-1 || line == size-2 || line == 2 || line == 3) && (x == (size-1) || x == 2)) || ((line == 2 || line == size-1) && (x == 3 || x == size-2)) = ' ' : (desenha_Mapa line size (x-1) (h:t))	
						 | (isBomb h || isFlames h || isBrick h) = '?' : desenha_Mapa line size (x-1) t	
						 | otherwise = ' ' : desenha_Mapa line size (x-1) t

mapa_aux :: Int -> Int -> Int -> String
mapa_aux  0 _ _ = []
mapa_aux  line size x | x == 1 = '#' : mapa_aux  (line-1) size size
					 | line == 1 || line == size || x == size || (mod line 2 == 1 && mod x 2 == 1) = '#' : (mapa_aux  line size (x-1))
					 | ((line == size-1 || line == size-2 || line == 2 || line == 3) && (x == (size-1) || x == 2)) || ((line == 2 || line == size-1) && (x == 3 || x == size-2)) = ' ' : (mapa_aux  line size (x-1))
					 | otherwise = ' ' : mapa_aux  line size (x-1) 
-------

--Usando a mecânica da fuñção drawMap, imprime numa lista de strings as coordenadas dos PUps (cada string é um PUp encontrado).
where_are_Bombs :: Int -> Int -> Int -> [Int] -> [String]
where_are_Bombs line size x [] = [] 
where_are_Bombs line size x (h:t) | x == 1 = where_are_Bombs (line-1) size size (h:t)
                                 | line == 1 || line == size || x == size || (mod line 2 == 1 && mod x 2 == 1) = where_are_Bombs line size (x-1) (h:t)
                                 | ((line == size-1 || line == size-2 || line == 2 || line == 3) && (x == (size-1) || x == 2)) || ((line == 2 || line == size-1) && (x == 3 || x == size-2)) = where_are_Bombs line size (x-1) (h:t)	
                                 | isBomb h =  ("+ " ++ show(size - x) ++ " " ++ show(size - line)) : where_are_Bombs line size (x-1) t	
                                 | otherwise = where_are_Bombs line size (x-1) t
  
where_are_Flames :: Int -> Int -> Int -> [Int] -> [String]
where_are_Flames line size x [] = [] 
where_are_Flames line size x (h:t) | x == 1 = where_are_Flames (line-1) size size (h:t)
                                   | line == 1 || line == size || x == size || (mod line 2 == 1 && mod x 2 == 1) = where_are_Flames line size (x-1) (h:t)
                                   | ((line == size-1 || line == size-2 || line == 2 || line == 3) && (x == (size-1) || x == 2)) || ((line == 2 || line == size-1) && (x == 3 || x == size-2)) = where_are_Flames line size (x-1) (h:t)	
                                   | isFlames h =  ("! " ++ show(size - x) ++ " " ++ show(size - line)) : where_are_Flames line size (x-1) t	
                                   | otherwise = where_are_Flames line size (x-1) t                                
  
--Divide uma string numa lista de strings, cada uma com n elementos (serve para dividir o mapa por linhas)
splitString :: Int -> String -> [String]
splitString n [] = []
splitString n string = take n string : splitString n (drop n string)

--Juntando as funções anteriores, imprime o mapa completo com as coordenadas dos PUps
mapa :: Int -> Int -> [String]
mapa n s = splitString n (desenha_Mapa n n n (aleatorio n s)) ++ (where_are_Bombs n n n (aleatorio n s)) ++ (where_are_Flames n n n (aleatorio n s))































------------- TAREFA 2 -------------------------------------------------------------------------------




type Ponto = (Int,Int)


mapa1 = ["#########","#       #","# #?#?# #","#     ? #","# # # #?#","# ?     #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5","* 2 1 0 1 10","* 3 5 2 1 10","0 2 3 +!","1 3 5 !","2 1 6 +"]

moveMaybe :: [String] -> Int -> Maybe Char -> [String]
moveMaybe mapa player (Just c) = move mapa player c
moveMaybe mapa player Nothing = mapa

move :: [String] -> Int -> Char -> [String]
move mapa player 'B' | is_it_playing mapa player && can_i_plantB mapa player = justMap mapa ++ new_Coordinates mapa (get_dir 'B' (get_me_player_coords (words (get_me_player_state mapa player)))) (get_me_tuples (coordinates_to_words (get_me_Coordinates mapa))) ++ all_bombs_players_state mapa player ++ all_players_state mapa player 'B'
                     | otherwise = mapa

move mapa player dir | is_it_playing mapa player && can_i_move mapa (get_dir dir (get_me_player_coords (words (get_me_player_state mapa player)))) = justMap mapa ++ new_Coordinates mapa (get_dir dir (get_me_player_coords (words (get_me_player_state mapa player)))) (get_me_tuples (coordinates_to_words (get_me_Coordinates mapa))) ++ get_me_Bombs_state mapa ++ all_players_state mapa player dir
                     | otherwise = mapa  


--FUNÇÃO RESPONSAVEL PELA APRESENTAÇAO DO MAPA APENAS----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- É A FUNÇÃO USADA NA MOVE:
justMap :: [String] -> [String]
justMap [] = []
justMap (h:t) | head h == '#' = [h] ++ justMap t
              | otherwise = []  

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--FUNÇÕES RESPONSAVEIS PELO ESTADO DAS COORDENADAS DOS POWER UPs-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

 --função que indica apenas as coordenadas dos Flames (em String):
get_me_Flame_Coordinates :: [String] -> [String]
get_me_Flame_Coordinates mapa = aux_Fcoord mapa where
  aux_Fcoord [] = []
  aux_Fcoord (h:t) | [head h] == "!" = [h] ++ aux_Fcoord t
                   | otherwise = aux_Fcoord t 

--função que indica apenas as coordenadas dos Bombs:
get_me_Bomb_Coordinates :: [String] -> [String]
get_me_Bomb_Coordinates mapa = aux_Bcoord mapa where
  aux_Bcoord [] = []
  aux_Bcoord (h:t) | [head h] == "+" = [h] ++ aux_Bcoord t
                   | otherwise = aux_Bcoord t 

-- É A FUNÇÃO USADA NA MOVE:
--função que junta as novas coordenadas Bombs e Flames:
new_Coordinates :: [String] -> Ponto -> [Ponto] -> [String]
new_Coordinates mapa (c,l) h = new_Bomb_Coordinates mapa (c,l) h ++ new_Flame_Coordinates mapa (c,l) h where
    new_Bomb_Coordinates mapa (c,l) h | elem (c,l) h = retirar ("! " ++ show c ++ " " ++ show l) (get_me_Flame_Coordinates mapa) 
                                      | otherwise = get_me_Bomb_Coordinates mapa
    new_Flame_Coordinates mapa (c,l) h | elem (c,l) h = retirar ("+ " ++ show c ++ " " ++ show l) (get_me_Bomb_Coordinates mapa)
                                       | otherwise = get_me_Flame_Coordinates mapa

---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---FUNÇÕES RESPONSAVEIS PELO ESTADO DO JOGADORES---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


--função que indica o TOTAL de estados de jogo (jogador movido + outros jogadores não movidos):
all_players_state :: [String] -> Int -> Char -> [String]
all_players_state [] _ _ = []
all_players_state mapa player dir = sort_players (new_player_state mapa player dir) (other_players_state (just_players_state mapa) player) where
    other_players_state [] _ = []
    other_players_state (h:t) player | [head h] /= show player = [h] ++ other_players_state t player
                                     | otherwise = other_players_state t player
    just_players_state [] = []
    just_players_state (h:t) | [head h] == "0" || [head h] == "1" || [head h] == "2" || [head h] == "3" = [h] ++ just_players_state t
                             | otherwise = just_players_state t
    new_player_state mapa player dir = [show player ++ " " ++ show (fst (get_dir dir (get_me_player_coords (words (get_me_player_state mapa player))))) ++ " " ++ show (snd (get_dir dir (get_me_player_coords (words (get_me_player_state mapa player))))) ++ " " ++ sort_ALL_player_power_ups mapa player dir]
    sort_ALL_player_power_ups mapa player dir = retirar '!' (player_ALL_power_ups mapa player dir) ++ aux_add_Flame (bomb_ray (player_ALL_power_ups mapa player dir)-1) 
    aux_add_Flame x | x==0 = []
                    | otherwise = "!" ++ aux_add_Flame (x-1)
    player_ALL_power_ups [] _ _ = []
    player_ALL_power_ups mapa player dir = already_had_power_ups mapa player ++ player_new_power_up_Bomb mapa player dir ++ player_new_power_up_Flame mapa player dir 
    player_new_power_up_Flame mapa player dir | elem (get_dir dir (get_me_player_coords (words (get_me_player_state mapa player)))) (get_me_tuples ( coordinates_to_words (get_me_Flame_Coordinates mapa))) = "!"
                                              | otherwise = []
    player_new_power_up_Bomb mapa player dir | elem (get_dir dir (get_me_player_coords (words (get_me_player_state mapa player)))) (get_me_tuples (coordinates_to_words (get_me_Bomb_Coordinates mapa))) = "+"
                                             | otherwise = []
    sort_players [(x:xs)] [] = [(x:xs)]
    sort_players [(x:xs)] ((y:ys):t) | (read [x] :: Int) < (read [y] :: Int) = [(x:xs)] ++ ((y:ys):t)
                                     | otherwise = [(y:ys)] ++ sort_players [(x:xs)] t


---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--FUNÇÕES RESPONSAVEIS PELO ESTADO DA BOMBA--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--função que indica apenas os estados de bomba (já plantadas no estado de jogo fornecido) em String:
get_me_Bombs_state :: [String] -> [String]
get_me_Bombs_state mapa = aux_Bomb_state mapa where
  aux_Bomb_state [] = []
  aux_Bomb_state (h:t) | [head h] == "*" = [h] ++ aux_Bomb_state t
                       | otherwise = aux_Bomb_state t 

--função que verifica se é possível o jogador plantar uma bomba:
can_i_plantB :: [String] -> Int -> Bool
can_i_plantB mapa player | elem (get_me_player_coords (words (get_me_player_state mapa player))) (bomb_planted_coordinates (coordinates_to_words (get_me_Bombs_state mapa))) = False
                         | otherwise = if how_many_bombs_can_i_plant (already_had_power_ups mapa player) > how_many_bombs_already_planted (coordinates_to_words (get_me_Bombs_state mapa)) player then True else False where
 bomb_planted_coordinates [] = []
 bomb_planted_coordinates ([a,b,c,d,e,f]:xs) = (read b :: Int,read c :: Int) : bomb_planted_coordinates xs
 how_many_bombs_can_i_plant [] = 1
 how_many_bombs_can_i_plant (x:xs) | x == '+' = 1 + how_many_bombs_can_i_plant xs
                                   | otherwise = how_many_bombs_can_i_plant xs
 how_many_bombs_already_planted [] _ = 0
 how_many_bombs_already_planted ([a,b,c,d,e,f]:xs) player | show player == d = 1 + how_many_bombs_already_planted xs player
                                                          | otherwise = how_many_bombs_already_planted xs player

all_bombs_players_state :: [String] -> Int -> [String]
all_bombs_players_state mapa player = reverse (retirar_repetidos (reverse (sort_all_bombs_player_state mapa (coordinates_to_words (plant_Bomb mapa player)) (coordinates_to_words (get_me_Bombs_state mapa))))) where
    plant_Bomb mapa player = ["* " ++ show (fst (get_me_player_coords (words (get_me_player_state mapa player)))) ++ " " ++ show (snd (get_me_player_coords (words (get_me_player_state mapa player)))) ++ " " ++ show player ++ " " ++ show (bomb_ray (already_had_power_ups mapa player)) ++ " 10"]
    sort_all_bombs_player_state mapa [[a,b,c,d,e,f]] [] = [unwords [a,b,c,d,e,f]] 
    sort_all_bombs_player_state mapa [[a,b,c,d,e,f]] ([g,h,i,j,k,l]:xs) | (read c :: Int) < (read i :: Int) = [unwords [a,b,c,d,e,f]] ++ get_me_Bombs_state mapa
                                                                        | (read c :: Int) > (read i :: Int) = [unwords [g,h,i,j,k,l]] ++ sort_all_bombs_player_state mapa [[a,b,c,d,e,f]] xs
                                                                        | (read c :: Int) == (read i :: Int) = if (read b :: Int) < (read h :: Int) then [unwords [a,b,c,d,e,f]] ++ get_me_Bombs_state mapa else [unwords [g,h,i,j,k,l]] ++ sort_all_bombs_player_state mapa [[a,b,c,d,e,f]] xs
    retirar_repetidos [] = []
    retirar_repetidos (x:xs) | elem x xs = retirar_repetidos xs
                             | elem x xs == False = x : retirar_repetidos xs
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---FUNÇÕES AUXILIARES USADAS NAS VÁRIAS FASES-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--função que indica os Power_Ups já capturados pelo jogador:
already_had_power_ups :: [String] -> Int -> String
already_had_power_ups mapa player = drop 6 (get_me_player_state mapa player)

--função que dando uma mapa retorna o estado de jogo de um jogador: (ex: " 0 4 3")
get_me_player_state :: [String] -> Int -> String
get_me_player_state mapa player = get_aux mapa player where
  get_aux (h:t) player | [head h] == show player = h
                       | otherwise = get_aux t player 

--função que recebe o estado de jogo de um jogador e indica as suas coordenadas num túpulo:
get_me_player_coords :: [String] -> Ponto
get_me_player_coords (a:b:c:d) = (read b :: Int,read c :: Int)

--função que indica o raio da bomba:
bomb_ray :: String -> Int
bomb_ray [] = 1
bomb_ray (x:xs) | x == '!' = 1 + bomb_ray xs
                 | otherwise = bomb_ray xs 

--converte as coordenadas de String para duplos:
get_me_tuples :: [[String]] -> [Ponto]
get_me_tuples [] = []
get_me_tuples ((a:b:c:d):xs) = (read b :: Int,read c :: Int) : get_me_tuples xs

--função auxiliar utilizada nas funções new_Bomb_Coordinates e new_Flame_Coordinates:
retirar :: Eq a => a -> [a] -> [a]
retirar a [] = []
retirar a (x:xs) = if a == x then retirar a xs else x : retirar a xs

--função auxiliar para converter as coordenadas de String para duplos (é utilizada também nos estados das Bombas):
coordinates_to_words :: [String] -> [[String]]
coordinates_to_words [] = []
coordinates_to_words (x:xs) = [words x] ++ coordinates_to_words xs 

get_me_Coordinates :: [String] -> [String]
get_me_Coordinates mapa = aux_coord mapa where
  aux_coord [] = []
  aux_coord (h:t) | [head h] == "+" || [head h] == "!" = [h] ++ aux_coord t
                  | otherwise = aux_coord t  

--função que dada as coordenadas de um jogador indica as novas coordenadas dependendo da direção:
get_dir :: Char -> Ponto -> Ponto
get_dir dir (c,l) | dir == 'U' = (c,l-1) 
                  | dir == 'D' = (c,l+1) 
                  | dir == 'L' = (c-1,l) 
                  | dir == 'R' = (c+1,l) 
                  | dir == 'B' = (c,l)

can_i_move :: [String] -> Ponto -> Bool 
can_i_move mapa (c,l) = if l_aux mapa (c,l) 0 == ' ' then True else False 
l_aux (x:xs) (c,l) l1 | l == l1 = c_aux x c 0
                      | otherwise = l_aux xs (c,l) (l1+1) 
c_aux (x:xs) c c1 | c == c1 = x 
                  | otherwise = c_aux xs c (c1+1)

is_it_playing :: [String] -> Int -> Bool
is_it_playing mapa player = get_aux2 mapa player where
 get_aux2 [] player = False 
 get_aux2 (h:t) player | [head h] == show player = True
                       | otherwise = get_aux2 t player















































------------- TAREFA 4 -------------------------------------------------------------------------------




mapa8 = ["###############","#     ??      #","# # #?# #?# # #","# ? ?     ????#","# # # #?# #?#?#","#??   ?  ?    #","# # # # # # # #","#? ? ?  ?     #","#?# # #?# # #?#","#   ????  ? ? #","# #?# # #?# # #","#?? ? ?? ?    #","# # #?# # #?# #","#      ? ? ?  #","###############","+ 1 7","+ 9 8","+ 6 11","! 3 4","! 11 7","! 12 7","! 13 8","* 1 1 3 2 1","* 3 3 1 3 1","* 10 7 1 3 1","* 4 13 0 1 1","* 5 13 2 2 1","0 1 2","1 2 9","2 4 13","3 3 8"]

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
raio_acao_de_todas_as_bombas mapa@(_,_,c,_) = concat $ aux1 mapa (reduz_Tempo_Bomba (quais_vao_ecplodir c)) where
    aux1 _ [] = []
    aux1 mapa (x:xs) = raio_acao_bomba mapa x : aux1 mapa xs

reduz_Tempo_Bomba :: [Estado_da_Bomba] -> [Estado_da_Bomba]
reduz_Tempo_Bomba [] = [] 
reduz_Tempo_Bomba ((Bomba_P (a,b,c,d,tempo)):xs) = (Bomba_P (a,b,c,d,tempo-1)) : reduz_Tempo_Bomba xs


--funcao que verifica se uma bomba vai ecplodir, em caso afirmativo retorna as bombas que vao esplodir:
quais_vao_ecplodir :: [Estado_da_Bomba] -> [Estado_da_Bomba]
quais_vao_ecplodir [] = [] 
quais_vao_ecplodir (bomba@(Bomba_P (_,_,_,_,tempo)):xs) | tempo == 1 = bomba : quais_vao_ecplodir xs
                                                        | otherwise = quais_vao_ecplodir xs

--Retorna os Estados das Bombas usados no mapa Atualizado (é o que vai ser usado na função avançca):
--testa se o estado de bomba em causa está dentro do raio de esplosão de outra bomba, em caso afirmativo reduz o temporizador dessa bomba passa para 1, de forma a forçar a sua ecplosão no próximo instante de tempo.
--em caso negativo retorna o mesmo estado de bomba:
atualiza_Bombas :: Estado_de_jogo -> Raio_Coordenadas -> [Estado_da_Bomba]
atualiza_Bombas (_,_,[],_) _ = []
atualiza_Bombas (a,b,e_b@(x:xs),d) r_lista = aux_retirar (quais_vao_ecplodir e_b) (aux_atualiza_Bombas x r_lista : atualiza_Bombas (a,b,xs,d) r_lista) where
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

---------------------------------------------------------------------------------------------------------
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
                             | elem (cv,lv) lista && w == 3 = aux4 lista (c,l) (cv,lv+1) (v+1) 0 -- canto superior direito 
                             | elem (cv,lv) lista && w == 0 = aux4 lista (c,l) (cv-1,lv) (v+1) 1 -- canto inferior direito 
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



































------------- TAREFA 6 -------------------------------------------------------------------------------


type Numero_jogador = Int
type Action = Char
type Bomba_Dist = (Char, Int)
type Jogada = (Action, Int)
type Jogadas = [Jogada]
type Possible_Action = (Action, Action)
type Distancia_em_Coordenada = (Int,Int)

--Função principal do bot:
--se não há perigo o bot ataca, caso contrário defende-se:
act :: Estado_de_jogo -> Numero_jogador -> Maybe Char
act state@(a,b,c,d) n | minSafetyLevel state (findPlayerCoords d n) == 10 = attack (power_ups_destapados state) n
                      | otherwise = evade (power_ups_destapados state) n


--Função que dirige o bot na direção dos powerUps:
attack :: Estado_de_jogo -> Numero_jogador -> Maybe Char
attack state@(a,b,c,d) n | nearestBrick state n /= (-1,-1) && nearestPowerUp state n == (-1,-1) = blowBricks state n
                         | nearestPowerUp state n /= (-1,-1) && nearestBrick state n == (-1,-1) = goTo state n (nearestPowerUp state n)
                         | nearestBrick state n == (-1,-1) && nearestPowerUp state n == (-1,-1) && distance (findPlayerCoords d n) (findMiddle a) == 0 = Nothing
                         | otherwise = goToMiddle state n 


--Função que comanda o bot para o meio do mapa
goToMiddle :: Estado_de_jogo -> Numero_jogador -> Maybe Char
goToMiddle state@(a,b,c,d) n = goTo state n (findMiddle a)


--Comanda o bot para alcançar e destruir o tijolo mais perto
blowBricks :: Estado_de_jogo -> Numero_jogador -> Maybe Char
blowBricks state@(a,b,c,d) n | goTo state n (nearestBrick state n) == evade state n = Just 'B'
                             | otherwise = goTo state n (nearestBrick state n) 


--Calcula a coordenada do tijolo mais próximo:
nearestBrick :: Estado_de_jogo -> Numero_jogador -> Coordenada
nearestBrick (a,b,c,d) n = nearest (findPlayerCoords d n) (listBrickCoords a 0)


--Lista as coordenadas dos tijolos:
listBrickCoords :: Mapa -> Int -> Lista_de_Coordenadas
listBrickCoords [] _ = []
listBrickCoords (h:t) l = aux h 0 l ++ listBrickCoords t (l+1)
  where aux [] _ _ = []
        aux (x:xs) c l | x == Tijolo = [(c,l)] ++ aux xs (c+1) l
                       | otherwise = aux xs (c+1) l

--Função que indica a coordenada do meio do mapa:
findMiddle :: Mapa -> Coordenada
findMiddle mapa = (mid, mid) where mid = (div (length mapa) 2) + 1 

--Dada uma distancia (em unidade de coordenada) indica o próximo passo (comando) seguro para chegar lá:
goTo :: Estado_de_jogo -> Numero_jogador -> Distancia_em_Coordenada -> Maybe Char
goTo state@(a,b,c,d) n (c1,c2) | c1 < 0 && (minSafetyLevel state (x-1,y) == 10) && (isValid a (findPlayer d n) 'L') = Just 'L'
                               | c1 > 0 && (minSafetyLevel state (x+1,y) == 10) && (isValid a (findPlayer d n) 'R') = Just 'R'
                               | c2 < 0 && (minSafetyLevel state (x,y-1) == 10) && (isValid a (findPlayer d n) 'U') = Just 'U'
                               | c2 > 0 && (minSafetyLevel state (x,y+1) == 10) && (isValid a (findPlayer d n) 'D') = Just 'D'
                               | otherwise = evade state n
                                 where (x,y) = findPlayerCoords d n



-------------------------------FUNÇÕES RESPONSÁVEIS PARA APANHAR POWER_UPS---------------------------------------------------------------------------------------------

         
--calcula a coordenada do Power Up mais próximo:
nearestPowerUp :: Estado_de_jogo -> Numero_jogador -> Coordenada
nearestPowerUp (a,[],c,d) _ = (-1,-1)
nearestPowerUp (a,b,c,d) n = nearest (findPlayerCoords d n) (puCoords b) 

--transforma o tipo Power_ups numa lista de coordenadas:
puCoords :: Power_Ups -> [Coordenada]
puCoords [] = []
puCoords ((PU_Bomba c):ts) = c : (puCoords ts)
puCoords ((PU_Flame c):ts) = c : (puCoords ts) 

--distância entre uma coordenada e a coordenada mais perto desta, entre uma lista de coordenadas:
nearest :: Coordenada -> [Coordenada] -> Coordenada
nearest _ [] = (-1,-1)
nearest a [x] = distCoords a x 
nearest a (h:t:ts) | distance a h < distance a t = nearest a (h:ts)
                   | otherwise = nearest a (t:ts)         

--distância entre duas coordenadas (unidade em número):
distance :: Coordenada -> Coordenada -> Int
distance (a,b) (c,d) =  abs (c-a) + abs (d-b)

--distância entre duas coordenadas (unidade em coordenadas):
distCoords :: Coordenada -> Coordenada -> Coordenada
distCoords (a,b) (c,d) = (c-a, d-b)

--serve para o reageEvento do bot na T5 para receber o mesmo estado_de_jogo mas apenas com os Power_Ups destapados, de forma a simular uma situação de jogo real:
power_ups_destapados :: Estado_de_jogo -> Estado_de_jogo
power_ups_destapados (a,b,c,d) = (a,aux a b,c,d) where
   aux _ [] = []
   aux a (x@(PU_Bomba coord):xs) | qual_o_caracter a coord == Vazio = x : aux a xs 
                                 | otherwise = aux a xs
   aux a (x@(PU_Flame coord):xs) | qual_o_caracter a coord == Vazio = x : aux a xs 
                                 | otherwise = aux a xs



-------------------------------FUNÇÕES RESPONSÁVEIS PARA A JOGADA DEFENSIVA (FUGIR ÁS BOMBAS)-------------------------------------------------------------------------------


--Dado o estado de jogo, devolve a jogada que deve ser efetuada pelo bot (defensiva):
evade :: Estado_de_jogo -> Numero_jogador -> Maybe Char
evade estado@(mapa, power_ups, bombas, jogadores) n = firstValid mapa (findPlayer jogadores n) (dirBySafety estado (findPlayerCoords jogadores n))

--Dado o número de um jogador, devolve o Jogador correspondente;
--NOTA: quando o jogador morre, esta função dá erro, uma vez que nao existe um caso especifico para quando o jogador nao é encontrado
--no entanto esse erro não interessa porque nos testes caso haja um erro o bot é eleminado, uma vez que o bot já estava morto não tem nehuma implicação ser eleminado:
findPlayer :: Jogadores -> Numero_jogador -> Jogador
findPlayer ((J j coords pus):js) n | j == n = (J j coords pus)
                                   | otherwise = findPlayer js n

--Dado o número de um jogador, devolve as coordenadas do Jogador correspondente:
findPlayerCoords :: Jogadores -> Numero_jogador -> Coordenada
findPlayerCoords jogadores n = let (J j coords pus) = findPlayer jogadores n 
                               in coords

--Verifica se uma acção é válida no mapa (se é possviel mover o jogador para essa coordenada):
isValid :: Mapa -> Jogador -> Char -> Bool
isValid map (J _ (x,y) pu ) c | c == 'U' && ((qual_o_caracter map (x, y - 1)) == Vazio) = True
                              | c == 'D' && ((qual_o_caracter map (x, y + 1)) == Vazio) = True
                              | c == 'L' && ((qual_o_caracter map (x - 1, y)) == Vazio) = True
                              | c == 'R' && ((qual_o_caracter map (x + 1, y)) == Vazio) = True
                              | c == 'B' && (length pu /= 0) = True
                              | otherwise = False

-- Dada a lista de direções e o nivel de perigo de cada, organizada por ordem crescente de perigo, seleciona a primeira jogada válida:
-- função que evoca a melhor jogada:
firstValid :: Mapa -> Jogador -> Jogadas -> Maybe Char
firstValid _ _ [] = Nothing
firstValid mapa player (h:ts) = let action = fst h
                                in if (isValid mapa player action) == True then Just action 
                                   else firstValid mapa player ts
                     

-- Funções que organizam uma lista de jogadas de forma crescente de perigo (decrescente de segurança):
decrescent :: Jogadas -> Jogadas
decrescent [x] = [x]
decrescent list = (decrescentAux1 list : decrescent (decrescentAux2 list))
 where decrescentAux2 [x] = []
       decrescentAux2 (h:s:ts) | (snd h) > (snd s) = s : (decrescentAux2 (h:ts))
                               | otherwise = h : (decrescentAux2 (s:ts))
       decrescentAux1 [x] = x
       decrescentAux1 (h:s:ts) | (snd h) > (snd s) = decrescentAux1 (h:ts)
                               | otherwise = decrescentAux1 (s:ts)


--Dado o estado de jogo e a posição de um jogador, devolve a lista de niveis de perigo das 4 direçoes, ordenada por perigo crescente
dirBySafety :: Estado_de_jogo -> Coordenada -> Jogadas
dirBySafety estado@(mapa,pu_s,bombs,jogadores) (cp, lp) = 
  decrescent [('L',minSafetyLevel estado (cp-1, lp)),('U',minSafetyLevel estado (cp, lp-1)),('R',minSafetyLevel estado (cp+1, lp)),('D',minSafetyLevel estado (cp, lp+1))] 


--Função usada ao longo de toda a T6:
--Função que calcula o perigo associado a uma coordenada do mapa, usando como factores o tempo que falta para as bombas ecplodirem
--e se estão no alcance da próxima jogada:
minSafetyLevel :: Estado_de_jogo -> Coordenada -> Int
minSafetyLevel state@(a,b,bombas,d) coord = minimum (safetyAux state coord bombas) 
  where safetyAux _ _ [] = [10]
        safetyAux estado coord (h:ts) = safetyLevel estado coord h : safetyAux estado coord ts


--verifica o nível de segurança de um jogador (0- jogador corre muito perigo ; 10 - o jogador não corre perigo):
safetyLevel :: Estado_de_jogo -> Coordenada -> Estado_da_Bomba -> Int
safetyLevel estado j_coords bomba@(Bomba_P (xb,yb,pl,range,time)) | withinRange estado j_coords bomba = time - 1
                                                                  | otherwise = 10


--verifica se o jogador está dentro do raio de ação de uma bomba:
withinRange :: Estado_de_jogo -> Coordenada -> Estado_da_Bomba -> Bool
withinRange estado j_coord bomba | elem j_coord (raio_acao_bomba estado bomba) = True
                                 | otherwise = False

