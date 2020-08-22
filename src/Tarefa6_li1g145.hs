module Tarefa6_li1g145 where

import AllF 

bot :: [String] -> Int -> Int -> Maybe Char
bot mapa player ticks = act (estado_de_jogo mapa) player

