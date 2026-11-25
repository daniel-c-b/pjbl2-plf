module Dados
  ( dadosCampeonato
  , campeonatoTeste
  , dadosComErros
  ) where

import Tipos

-- campeonato em texto
-- linhas de comentario (prefixadas com #) e linhas vazias sao ignoradas
dadosCampeonato :: String
dadosCampeonato = unlines
  [ "# rodada;mandante;gols mandante;gols visitante;visitante"
  , "1;Coritiba;4;0;Londrina"
  , "1;Athletico;1;0;Cianorte"
  , "1;Paraná;0;0;Operário"
  , ""
  , "2;Coritiba;2;1;Cianorte"
  , "2;Londrina;1;1;Operário"
  , "2;Athletico;2;0;Paraná"
  , "3;Coritiba;0;1;Operário"
  , "3;Cianorte;2;2;Paraná"
  , "3;Londrina;0;2;Athletico"
  , "4;Coritiba;1;1;Paraná"
  , "4;Operário;1;0;Athletico"
  , "4;Cianorte;3;1;Londrina"
  , "5;Coritiba;0;1;Athletico"
  , "5;Paraná;2;0;Londrina"
  , "5;Operário;1;2;Cianorte"
  ]

-- o campeonato acima mas em objetos de partida
campeonatoTeste :: [Partida]
campeonatoTeste =
  [ Partida 1 "Coritiba" 4 0 "Londrina"
  , Partida 1 "Athletico" 1 0 "Cianorte"
  , Partida 1 "Paraná" 0 0 "Operário"
  , Partida 2 "Coritiba" 2 1 "Cianorte"
  , Partida 2 "Londrina" 1 1 "Operário"
  , Partida 2 "Athletico" 2 0 "Paraná"
  , Partida 3 "Coritiba" 0 1 "Operário"
  , Partida 3 "Cianorte" 2 2 "Paraná"
  , Partida 3 "Londrina" 0 2 "Athletico"
  , Partida 4 "Coritiba" 1 1 "Paraná"
  , Partida 4 "Operário" 1 0 "Athletico"
  , Partida 4 "Cianorte" 3 1 "Londrina"
  , Partida 5 "Coritiba" 0 1 "Athletico"
  , Partida 5 "Paraná" 2 0 "Londrina"
  , Partida 5 "Operário" 1 2 "Cianorte"
  ]

-- partidas com erro
dadosComErros :: String
dadosComErros = unlines
  [ "# rodada;mandante;gols mandante;gols visitante;visitante"
  , "1;Coritiba;dois;1;Athletico"
  , "1;Paraná;0;0;Paraná"
  , "1;Operário;1;-1;Cianorte"
  , "2;Londrina;1;0"
  , ""
  , "2;Coritiba;1;0;Athletico"
  , "2;Coritiba;2;2;Cianorte"
  ]
