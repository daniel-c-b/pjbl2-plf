-- Camada 2
module Classificacao
  ( pontos
  , linhaDoTime
  , pontosLinha
  , saldo
  , aproveitamento
  , classificacao
  , ateRodada
  , maiorGoleada
  , mediaGols
  ) where

import Data.List (sortBy)
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..), comparing)

import Tipos

-- pontos que dependem de resultado
pontos :: Resultado -> Int
pontos Vitoria = 3
pontos Empate  = 1
pontos Derrota = 0

linhaVazia :: Time -> Linha
linhaVazia t = Linha t 0 0 0 0 0 0

-- acumula uma partida à linha do time
acumular :: Time -> Linha -> Partida -> Linha
acumular t l p = maybe l atualizar (resultadoPara t p)
  where
    (pro, contra)
      | t == mandante p = (golsMandante p, golsVisitante p)
      | otherwise       = (golsVisitante p, golsMandante p)
    atualizar r = l
      { jogos      = jogos l + 1
      , vitorias   = vitorias l + conta Vitoria r
      , empates    = empates l + conta Empate r
      , derrotas   = derrotas l + conta Derrota r
      , golsPro    = golsPro l + pro
      , golsContra = golsContra l + contra
      }
    conta alvo r = if r == alvo then 1 else 0

linhaDoTime :: [Partida] -> Time -> Linha
linhaDoTime ps t = foldl (acumular t) (linhaVazia t) ps

pontosLinha :: Linha -> Int
pontosLinha l =
  pontos Vitoria * vitorias l + pontos Empate * empates l + pontos Derrota * derrotas l

-- saldo de gols
saldo :: Linha -> Int
saldo l = golsPro l - golsContra l

-- percentual de pontos em uma Linha
-- 0 pontos caso nao tenha jogado
aproveitamento :: Linha -> Double
aproveitamento l
  | jogos l == 0 = 0
  | otherwise    = 100 * fromIntegral (pontosLinha l) / fromIntegral (pontos Vitoria * jogos l)

-- criterios de ordenaçao
criterio :: Linha -> Linha -> Ordering
criterio = comparing (Down . pontosLinha)
        <> comparing (Down . vitorias)
        <> comparing (Down . saldo)
        <> comparing (Down . golsPro)
        <> comparing time

-- tabela ordenada
classificacao :: [Partida] -> [Linha]
classificacao ps = sortBy criterio (map (linhaDoTime ps) (times ps))

-- partidas ate uma determinada rodada
ateRodada :: Int -> [Partida] -> [Partida]
ateRodada n = filter ((<= n) . rodada)

diferencaGols :: Partida -> Int
diferencaGols p = abs (golsMandante p - golsVisitante p)

-- partida com maior diferença de gols
maiorGoleada :: [Partida] -> Maybe Partida
maiorGoleada = listToMaybe . sortBy (comparing (Down . diferencaGols))

-- media de gols em uma partida
mediaGols :: [Partida] -> Maybe Double
mediaGols [] = Nothing
mediaGols ps = Just (fromIntegral total / fromIntegral (length ps))
  where
    total = sum (map (\p -> golsMandante p + golsVisitante p) ps)
