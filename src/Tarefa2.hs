import Data.Char (isDigit)
import System.Environment
import Data.List
type Ponto = (Int,Int)

mapa2 = ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 3 3","! 5 5","* 7 7 1 1 10","0 4 3 +","1 7 7"]

mapa1 = ["#########","#       #","# #?# # #","#     ? #","# # # #?#","# ?     #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5","* 2 1 0 1 10","* 3 5 2 1 10","0 2 3 +!","1 3 5 !","2 1 6 +"]

main :: IO ()
main = do a <- getArgs
          let p = a !! 0
          let c = a !! 1
          w <- getContents
          if length a == 2 && length p == 1 && isDigit (head p) && length c == 1 && head c `elem` "UDLRB"
             then putStr $ unlines $ move (lines w) (read p) (head c)
             else putStrLn "Parâmetros inválidos"

move :: [String] -> Int -> Char -> [String]
move mapa player 'B' | is_it_playing mapa player && can_i_plantB mapa player = justMap mapa ++ new_Coordinates mapa (get_dir 'B' (get_me_player_coords (words (get_me_player_state mapa player)))) (get_me_tuples (coordinates_to_words (get_me_Coordinates mapa))) ++ all_bombs_players_state mapa player ++ all_players_state mapa player 'B'
                     | otherwise = mapa

move mapa player dir | is_it_playing mapa player && can_i_move mapa (get_dir dir (get_me_player_coords (words (get_me_player_state mapa player)))) = justMap mapa ++ new_Coordinates mapa (get_dir dir (get_me_player_coords (words (get_me_player_state mapa player)))) (get_me_tuples (coordinates_to_words (get_me_Coordinates mapa))) ++ get_me_Bombs_state mapa ++ all_players_state mapa player dir
                     | otherwise = mapa  


--FUNÇÃO RESPONSAVEL PELA APRESENTAÇAO DO MAPA APENAS----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- É A FUNÇÃO USADA NA MOVE:
justMap :: [String] -> [String]
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