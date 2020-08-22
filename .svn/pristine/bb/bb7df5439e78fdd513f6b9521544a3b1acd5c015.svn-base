module Main where

import AllF
import Tarefa6_li1g145
import Graphics.Gloss         
import Graphics.Gloss.Data.Picture  
import Graphics.Gloss.Interface.Pure.Game

-- | Uma representação do estado do jogo.
type Estado = (Numero_jogador,Dimensao_do_mapa,Estado_de_jogo,Figuras,Time,Var)

type Dimensao_do_mapa = Int
type Time = Float
type Var = Float

data Figuras = Figuras { tijolo :: Picture, pedra :: Picture, dirt :: Picture, jogador0 :: Picture, jogador1 :: Picture, jogador2 :: Picture, jogador3 :: Picture, bomba :: Picture, pubomba :: Picture, puflame :: Picture, aba :: Picture, logo :: Picture, relogio :: Picture, preto :: Picture, raio :: Picture }

--data Figuras = Figuras { tijolo :: Picture, pedra :: Picture, dirt :: Picture, jogador0 :: Picture, jogador1 :: Picture, jogador2 :: Picture, jogador3 :: Picture, bomba :: Picture, pubomba :: Picture, puflame :: Picture, aba :: Picture, logo :: Picture, j0 :: Picture, j1 :: Picture, j2 :: Picture, j3 :: Picture }

-- | O estado inicial do jogo.
estadoInicial :: IO Estado
estadoInicial = do
  fig1 <- loadBMP "t5_imagens/tijolo.bmp"
  fig2 <- loadBMP "t5_imagens/pedra.bmp"
  fig3 <- loadBMP "t5_imagens/dirt.bmp"
  fig4 <- loadBMP "t5_imagens/jogador0.bmp"
  fig5 <- loadBMP "t5_imagens/jogador1.bmp"
  fig6 <- loadBMP "t5_imagens/jogador2.bmp"
  fig7 <- loadBMP "t5_imagens/jogador3.bmp"
  fig8 <- loadBMP "t5_imagens/bomba.bmp"
  fig9 <- loadBMP "t5_imagens/pubomba.bmp"
  fig10 <- loadBMP "t5_imagens/puflame.bmp"
  fig11 <- loadBMP "t5_imagens/aba.bmp"
  fig12 <- loadBMP "t5_imagens/bombermanlogo.bmp"
  fig13 <- loadBMP "t5_imagens/relogio.bmp"
  fig14 <- loadBMP "t5_imagens/preto.bmp"
  fig15 <- loadBMP "t5_imagens/raio.bmp"
  let l_figuras = Figuras fig1 fig2 fig3 fig4 fig5 fig6 fig7 fig8 fig9 fig10 fig11 fig12 fig13 fig14 fig15
  putStrLn "Qual a Dimensão?"
  x <- getLine
  let size = read x :: Dimensao_do_mapa 
  putStrLn "Qual a Seed?"
  y <- getLine
  let seed = read y :: Int
  putStrLn "Qual o jogador que pretende controlar?"
  z <- getLine
  let jog = read z :: Numero_jogador 
  let (a,b,c,d) = estado_de_jogo $ mapa size seed :: Estado_de_jogo
  let time = 20 + (fromIntegral $ size-2)^2 :: Time
  return (jog,size,(a,b,c,[J 0 (1,1) "",J 1 (size-2,1) "",J 2 (1,size-2) "",J 3 (size-2,size-2) ""]),l_figuras,time,0) --retrun estado


---------------------------------------------------------------------------------------------------------------------------------

-- | Função que desenha o jogo.
desenhaEstado :: Estado -> Picture
desenhaEstado estado@(jog,dim,(a,b,c,d),figuras,time,var) = Translate (-k) k $ Pictures $ junta_tudo estado where k = 60 * (fromIntegral (div dim 2))

--função que organiza a ordem de cada figura no mapa (Pedra,Pedra,etc) consoante o estado de jogo:
aux_get_figuras :: ([Celulas],Figuras) -> [Picture]
aux_get_figuras ([],_) = []
aux_get_figuras ((x:xs),figuras) | x == Pedra = pedra figuras : aux_get_figuras (xs,figuras)
                                 | x == Vazio = dirt figuras : aux_get_figuras (xs,figuras)
                                 | x == Tijolo = tijolo figuras : aux_get_figuras (xs,figuras)

--função que desenha apenas o tabuleiro do mapa com Pedra, Tijolo e Vazio (faltam os jogadores, Power Ups e bombas plantadas:
desenha_mapa :: Dimensao_do_mapa -> Coordenada -> [Picture] -> [Picture] 
desenha_mapa _ _ [] = []
desenha_mapa dim (c,l) (x:xs) | c == dim = desenha_mapa dim (0,(l+1)) (x:xs)
                              | otherwise = desenha_coord (c,l) x : desenha_mapa dim ((c+1),l) xs

--função que coloca a figura na sua coordenada correta no referencial:
desenha_coord :: Coordenada -> Picture -> Picture
desenha_coord (c,l) figura = Translate (60*(fromIntegral c)) (-60*(fromIntegral l)) figura 

aux_put_J :: Estado -> [Picture]
aux_put_J (_,_,(_,_,_,[]),_,_,_) = []
aux_put_J (jog,dim,(a,b,c,(J num coord _:xs)),figuras,time,var) | num == 0 = desenha_coord coord (jogador0 figuras) : aux_put_J (jog,dim,(a,b,c,xs),figuras,time,var)
                                                                | num == 1 = desenha_coord coord (jogador1 figuras) : aux_put_J (jog,dim,(a,b,c,xs),figuras,time,var)
                                                                | num == 2 = desenha_coord coord (jogador2 figuras) : aux_put_J (jog,dim,(a,b,c,xs),figuras,time,var)
                                                                | num == 3 = desenha_coord coord (jogador3 figuras) : aux_put_J (jog,dim,(a,b,c,xs),figuras,time,var)

aux_put_B :: Estado -> [Picture]
aux_put_B (_,_,(_,_,[],_),_,_,_) = []
aux_put_B (jog,dim,estado@(a,b,bomba_P@(Bomba_P (c,l,j,r,t)):xs,d),figuras,time,var) 
    | t == 1 = [desenha_coord (c,l) (bomba figuras)] ++ aux (raio_acao_bomba estado bomba_P) (raio figuras) ++ aux_put_B (jog,dim,(a,b,xs,d),figuras,time,var) 
    | otherwise = [desenha_coord (c,l) (bomba figuras)] ++ aux_put_B (jog,dim,(a,b,xs,d),figuras,time,var)
      where aux [] _ = []
            aux (x:xs) raio_fig = [desenha_coord x raio_fig] ++ aux xs raio_fig 



aux_put_PU :: Estado -> [Picture]
aux_put_PU (_,_,(_,[],_,_),_,_,_) = []
aux_put_PU (jog,dim,(a,(x:xs),c,d),figuras,time,var) 
   | is_it_PU_Flame x && is_it_vazio (concat a) (pu_coords x) = [desenha_coord (pu_coords x) (puflame figuras)] ++ aux_put_PU (jog,dim,(a,xs,c,d),figuras,time,var)
   | is_it_PU_Flame x == False && is_it_vazio (concat a) (pu_coords x) = [desenha_coord (pu_coords x) (pubomba figuras)] ++ aux_put_PU (jog,dim,(a,xs,c,d),figuras,time,var)
   | otherwise = [desenha_coord (pu_coords x) (tijolo figuras)] ++ aux_put_PU (jog,dim,(a,xs,c,d),figuras,time,var)
      where is_it_PU_Flame (PU_Flame _) = True 
            is_it_PU_Flame (PU_Bomba _) = False
            is_it_vazio mapa coord | (!!) mapa ((aux_count coord dim)-1) == Vazio = True
                                   | otherwise = False
            aux_count (c,l) dim = l * dim + c + 1


--inicio: (c,l) = (dim+1,0) 
--desenha a aba:
desenha_aba :: Coordenada -> Estado -> [Picture] 
desenha_aba (c,l) estado@(jog,dim,(a,b,e,d),figuras,time,var) = 
  aux_aba dim (c,l) figuras ++ [aux_logotipo dim figuras] ++ aux_cruz (aux_jogadores_aba dim figuras) (quem_esta_a_jogar estado) ++ [Translate 15 0 $ aux_time dim time figuras] where
        aux_aba dim (c,l) figuras | l == dim = []
                                  | otherwise = desenha_coord (c,l) (aba figuras) : aux_aba dim (c,l+1) figuras
        aux_logotipo dim figuras = Pictures [Translate 0 40 $ desenha_coord (dim+1,1) (logo figuras)]
        aux_jogadores_aba dim figuras =
               [Translate (-38) 10 $ desenha_coord (dim+1,2) (Scale 0.6 0.6 (jogador0 figuras)),
                Translate 38 10 $ desenha_coord (dim+1,2) (Scale 0.6 0.6 (jogador1 figuras)),
                Translate (-38) 5 $ desenha_coord (dim+1,3) (Scale 0.6 0.6 (jogador2 figuras)),
                Translate 38 5 $ desenha_coord (dim+1,3) (Scale 0.6 0.6 (jogador3 figuras))]
        aux_cruz _ [] = []
        aux_cruz [a,b,c,d] (x:xs) | x == 0 = [a] ++ aux_cruz [a,b,c,d] xs
                                  | x == 1 = [b] ++ aux_cruz [a,b,c,d] xs
                                  | x == 2 = [c] ++ aux_cruz [a,b,c,d] xs
                                  | x == 3 = [d] ++ aux_cruz [a,b,c,d] xs
        aux_time dim time figuras = Pictures [Translate 45 2 $ desenha_coord (dim,4) (Scale 1.15 1 $ preto figuras),
                                     Translate 25 (-10) $ desenha_coord (dim,4) (Color white $ Scale 0.25 0.25 $ Text $ show $ time),
                                     Translate (-5) 2 $ desenha_coord (dim,4) (relogio figuras)]


quem_esta_a_jogar :: Estado -> [Numero_jogador]
quem_esta_a_jogar (_,_,(_,_,_,[]),_,_,_) = []
quem_esta_a_jogar (jog,dim,(a,b,c,((J n y w):xs)),figuras,time,var) = [n] ++ quem_esta_a_jogar (jog,dim,(a,b,c,xs),figuras,time,var) 


junta_tudo :: Estado -> [Picture]
junta_tudo estado@(jog,dim,(a,b,c,d),figuras,time,var) = 
   desenha_mapa dim (0,0) (aux_get_figuras (concat a,figuras)) ++ aux_put_PU estado ++ aux_put_J estado ++ aux_put_B estado ++ desenha_aba (dim+1,0) estado

---------------------------------------------------------------------------------------------------------------------------------

-- | Função que altera o estado do jogo quando acontece um evento.
reageEvento :: Event -> Estado -> Estado
reageEvento (EventKey (Char 'w') Down _ _) (jog,dim,mapa,figuras,time,var) = (jog,dim,(estado_de_jogo $ move (reverte_Estado_de_jogo mapa) jog 'U'),figuras,time,var)
reageEvento (EventKey (Char 's') Down _ _) (jog,dim,mapa,figuras,time,var) = (jog,dim,(estado_de_jogo $ move (reverte_Estado_de_jogo mapa) jog 'D'),figuras,time,var)
reageEvento (EventKey (Char 'a') Down _ _) (jog,dim,mapa,figuras,time,var) = (jog,dim,(estado_de_jogo $ move (reverte_Estado_de_jogo mapa) jog 'L'),figuras,time,var)
reageEvento (EventKey (Char 'd') Down _ _) (jog,dim,mapa,figuras,time,var) = (jog,dim,(estado_de_jogo $ move (reverte_Estado_de_jogo mapa) jog 'R'),figuras,time,var)
reageEvento (EventKey (Char 'b') Down _ _) (jog,dim,mapa,figuras,time,var) = (jog,dim,(estado_de_jogo $ move (reverte_Estado_de_jogo mapa) jog 'B'),figuras,time,var)
reageEvento (EventKey _ _ _ _) (jog,dim,mapa,figuras,time,var) = (jog,dim,(estado_de_jogo $ moveMaybe (reverte_Estado_de_jogo mapa) 3 (bot (reverte_Estado_de_jogo $ power_ups_destapados mapa) 3 (truncate time))),figuras,time,var) --esta linha depois no final de tudo vai ser retirada!
reageEvento _ mapa = mapa -- ignora qualquer outro evento

-------------------------------------------------------------------------------------------------------------------------------

-- | Função que altera o estado do jogo quando o tempo avança n segundos.
reageTempo :: Float -> Estado -> Estado
reageTempo f (jog,dim,estado,figuras,time,var) | k >= 1 = (jog,dim,estado_de_jogo $ avanca (reverte_Estado_de_jogo estado) (truncate (time-1)),figuras,(time-1),0)
                                               | otherwise = (jog,dim,estado,figuras,time,k) 
                                                 where k = var + f

-- | Frame rate
fr :: Int
fr = 60

-- | Display mode
dm :: Display
dm = InWindow "Bomberman v2.0 ---> Made By: Ricardo Petronilho & Joaquim Simões!" (800, 800) (0, 0)
    
-- | Função principal que invoca o jogo.
main :: IO ()
main = do inicio <- estadoInicial 
          play dm              -- display mode
               black           -- côr do fundo da janela
               fr              -- frame rate
               inicio          -- estado inicial
               desenhaEstado   -- desenha o estado do jogo
               reageEvento     -- reage a um evento
               reageTempo      -- reage ao passar do tempo

