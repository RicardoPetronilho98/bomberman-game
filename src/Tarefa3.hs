import System.Environment
import Data.Char
import Data.List

decode :: String -> [String]
decode l = decode_only_map (get_only_encoded_map l) ++ get_it (not_get_only_encoded_map l)

encode :: [String] -> String
encode mapa = (show (get_size mapa)) ++ " " ++ reverse (drop 1 (reverse (retirar '0' (concat (f4 mapa (empty_QT (remove_pedras (justMap mapa)))))))) ++ " " ++ delimita (not_map mapa)

delimita :: [String] -> String 
delimita [] = []
delimita (h:t) = h ++ "P" ++ delimita t





mapa2 = ["###############","#     ??      #","# # #?# #?# # #","# ? ?     ????#","# # # #?# #?#?#","#??   ?  ?    #","# # # # # # # #","#? ? ?  ?   ? #","#?# # #?# # #?#","#   ????  ? ? #","# #?# # #?# # #","#?? ? ?? ?    #","# # #?# # #?# #","#      ? ? ?  #","###############","+ 7 1","+ 1 5","+ 2 5","+ 1 10","+ 6 11","! 3 7","! 12 7","! 13 8","! 7 9","* 3 8 3 1 10","* 4 13 0 1 10","0 9 1 +","1 2 9 !!","2 4 13","3 3 8"]

mapa1 = ["#########","#       #","# #?# # #","#     ? #","# # # #?#","# ?     #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5","* 2 1 0 1 10","* 3 5 2 1 10","0 2 3 +!","1 3 5 !","2 1 6 +"] --mapa original

mapa7 = ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######"]

mapa11 = ["###########","#     ??  #","# # # #?# #","#  ? ?  ? #","#?# # #?# #","# ????   ?#","# #?#?#?#?#","#   ?  ?  #","# # # # # #","#         #","###########"]



main :: IO ()
main = do a <- getArgs
          let p = a !! 0
          w <- getContents
          if length a == 1 && length p == 2 && (p=="-e" || p=="-d")
             then if p=="-e" then putStr $ encode $ lines w
                             else putStr $ unlines $ decode w
             else putStrLn "Parâmetros inválidos"







--["+ 5 2","+ 3 3","! 5 5","* 2 1 0 1 10","* 3 5 2 1 10","0 2 3 +!","1 3 5 !","2 1 6 +"]











--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--ENCODE APENAS O MAPA:

-- contrário da justMap:
not_map :: [String] -> [String]
not_map [] = []
not_map (h:t) | head h == '#' = not_map t
              | otherwise = h : not_map t


--função que indica a dimensão do mapa:
get_size :: [String] -> Int
get_size mapa = length (head mapa)

--função que apenas apresenta o mapa:
justMap :: [String] -> [String]
justMap [] = []
justMap (h:t) | head h == '#' = [h] ++ justMap t
              | otherwise = []  

--função que retira um elemento a uma lista:
retirar :: Eq a => a -> [a] -> [a]
retirar a [] = []
retirar a (x:xs) = if a == x then retirar a xs else x : retirar a xs

--função 1º:
--função que remove as pedras ao mapa, indicando a dimensão do mapa ao inicio numa String:
remove_pedras :: [String] -> [String]
remove_pedras mapa = lines (retirar '#' (unlines mapa))

--função 2º:
--função que indica a quantidade de espaços vazios que existe em cada linha até ao ponto de interrogação e depois...
empty_QT :: [String] -> [String]
empty_QT [] = []
empty_QT (x:xs) = [empty_spaces 0 x ++ "/"] ++ empty_QT xs where -- '/' serve para separar as linhas
  empty_spaces n [] = show n 
  empty_spaces n (x:xs) | x == ' ' = empty_spaces (n+1) xs
                        | x == '?' = if n==0 then '?' : empty_spaces 0 xs else show n ++ '?' : empty_spaces 0 xs
                        
f4 :: [String] -> [String] -> [String]
f4 _ [] = []
f4 mapa (x:xs) = [(concat (get_dot (aux_union' (get_size mapa) (separate x))))] ++ f4 mapa xs


separate :: String -> [String]
separate [] = []
separate (x:xs) = [[x]] ++ separate xs
--["5","?","?","6","/"]

union' :: [String] -> [String]
union' [x] = [x]
union' (x:y:t) | x == y = [x++y] ++ union' t 
               | otherwise = [x] ++ union' (y:t)
  
--função que aplica a mesma função "x" (size-2) vezes: 
aux_union' :: Int -> [String] -> [String]
aux_union' 1 y = union' y 
aux_union' x y = aux_union' (x-1) a where
  a = union' y
--["1","?","1","?","5","????","0","/"]

get_dot :: [String] -> [String] 
get_dot [] = []
get_dot (x:xs) | head x == '?' = [aux_1 x] ++ get_dot xs
               | otherwise = [x] ++ get_dot xs where 
                aux_1 h | length h == 1 = h
                        | length h == 2 = "A"  
                        | otherwise = " " ++ show (length h) ++ " " 









------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Decode apenas o mapa:

-- n é o size (get_size_decode mapa):
decode_only_map :: String -> [String]
decode_only_map [] = [] 
decode_only_map mapa = [replicate (get_size_decode mapa) '#'] ++ put_hashtag_even_Final (lines (reverse (drop 2 (reverse (drop 2 (split_hashtag (aux_decode_1 (words (get_only_map_decode mapa))))))))) 0 (div (get_size_decode mapa) 2 - 1) ++ [replicate (get_size_decode mapa) '#']


get_size_decode :: String -> Int
get_size_decode mapa = (read (head (words mapa)) :: Int) 

get_only_map_decode :: String -> String
get_only_map_decode mapa = reverse (drop 1 (reverse (drop ((length (show (get_size_decode mapa))) +1) (aux_count mapa 0 (get_size_decode mapa))))) where 
	aux_count [] _ _ = []
	aux_count (x:xs) n size | n == size = [x]
                          | otherwise = if x == '/' then [x] ++ aux_count xs (n+1) size else [x] ++ aux_count xs n size


aux_decode_1 :: [String] -> String
aux_decode_1 [] = []
aux_decode_1 (x:xs) | length x == 1 = aux2 x ++ aux_decode_1 xs
                    | otherwise = aux1 x ++ aux_decode_1 xs 

aux1 :: String -> String
aux1 [] = []
aux1 (x:xs) | x == '/' = "#" ++ aux1 xs
            | x == '?' = [x] ++ aux1 xs
            | x == 'A' = "??" ++ aux1 xs
            | otherwise = replicate (read [x] :: Int) ' ' ++ aux1 xs

aux2 :: String -> String
aux2 x = replicate (read x :: Int) '?'
-- "#       # ?  #     ? #   ?# ?     # ?? #  ??   #"


split_hashtag :: String -> String
split_hashtag [] = []
split_hashtag (x:xs) | x == '#' = [x] ++ "\n" ++ [x] ++ split_hashtag xs  
                     | otherwise = [x] ++ split_hashtag xs
--nao esqueçer de fazer o reverse -- drop 2 -- reverse -- drop 2

--"#       #\n# ?  #\n#     ? #\n#   ?#\n# ?     #\n# ?? #\n#  ??   #"

--lines -------- ["#       #","# ?  #","#     ? #","#   ?#","# ?     #","# ?? #","#  ??   #"]


--dim== (div 9 2 - 1)
--p começa em 0
--n começa em 0
put_hashtag_even_Final :: [String] -> Int -> Int -> [String] 
put_hashtag_even_Final [] _ _ = []
put_hashtag_even_Final (h:t) p dim | odd p = put_hashtag_even [(reverse (drop 1 (reverse (drop 1 h))))] dim 0 ++ put_hashtag_even_Final t (p+1) dim
                                   | otherwise = [h] ++ put_hashtag_even_Final t (p+1) dim 
--["#       #","# #?# # #","#     ? #","# # # #?#","# ?     #","# #?#?# #","#  ??   #"]
 
put_hashtag_even :: [String] -> Int -> Int -> [String]
put_hashtag_even [] _  _ = []
put_hashtag_even (h:t) dim n | odd n = ("#" ++ h ++ "#") : put_hashtag_even t dim (n+1)
                             | otherwise = ("#" ++ (intersperse '#' h) ++ "#") : put_hashtag_even t dim (n+1)



get_only_encoded_map :: String -> String
get_only_encoded_map mapa = reverse (aux_get (get_dim mapa) [] 1 mapa) ++ " "
	where
	aux_get :: Int -> String -> Int -> String -> String
 	aux_get dim w n (h:t) | n == dim = w
						  | otherwise = if h == '/' then aux_get dim (h:w) (n+1) t else aux_get dim (h:w) n t


get_dim mapa = read (aux mapa) :: Int where
    aux (h:t) | h /= ' ' = h : aux t 
              | otherwise = []


not_get_only_encoded_map :: String -> String
not_get_only_encoded_map mapa = tail $ reverse $ aux_get (reverse mapa)
	where aux_get (h:t) = if h == '/' then [] else h : aux_get t



get_it :: String -> [String]
get_it list = aux_it [] list

aux_it _ [] = []
aux_it z (h:t) | h == 'P' = [z] ++ aux_it [] t
               | otherwise = aux_it (z++[h]) t  



