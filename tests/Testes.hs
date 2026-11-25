module Main (main) where

import Classificacao
import Dados
import Leitura
import Tipos

testar :: (Eq a, Show a) => String -> a -> a -> IO ()
testar nome obtido esperado
  | obtido == esperado = putStrLn ("OK " ++ nome)
  | otherwise          = putStrLn ("FALHOU " ++ nome ++ ": " ++ show obtido)

exemplo :: Partida
exemplo = Partida 1 "Coritiba" 2 1 "Athletico"

esperadaFinal :: [Linha]
esperadaFinal =
  [ Linha "Athletico" 5 4 0 1 6 1
  , Linha "Operário"  5 2 2 1 4 3
  , Linha "Coritiba"  5 2 1 2 7 4
  , Linha "Cianorte"  5 2 1 2 8 7
  , Linha "Paraná"    5 1 3 1 5 5
  , Linha "Londrina"  5 0 1 4 2 12
  ]

esperadaRodada3 :: [Linha]
esperadaRodada3 =
  [ Linha "Athletico" 3 3 0 0 5 0
  , Linha "Coritiba"  3 2 0 1 6 2
  , Linha "Operário"  3 1 2 0 2 1
  , Linha "Paraná"    3 0 2 1 2 4
  , Linha "Cianorte"  3 0 1 2 3 5
  , Linha "Londrina"  3 0 1 2 1 7
  ]

main :: IO ()
main = do
  putStrLn "Camada 1:"
  testar "resultadoPara mandante" (resultadoPara "Coritiba" exemplo) (Just Vitoria)
  testar "resultadoPara visitante" (resultadoPara "Athletico" exemplo) (Just Derrota)
  testar "resultadoPara time que não jogou" (resultadoPara "Paraná" exemplo) Nothing
  testar "resultadoPara empate" (resultadoPara "Paraná" (Partida 1 "Paraná" 0 0 "Operário")) (Just Empate)
  testar "participou" (map (`participou` exemplo) ["Coritiba", "Athletico", "Paraná"]) [True, True, False]
  testar "times sem repetição e em ordem alfabética" (times campeonatoTeste)
    ["Athletico", "Cianorte", "Coritiba", "Londrina", "Operário", "Paraná"]
  testar "times de campeonato vazio" (times []) []

  putStrLn "Camada 2:"
  testar "pontos" (map pontos [Vitoria, Empate, Derrota]) [3, 1, 0]
  testar "classificacao completa" (classificacao campeonatoTeste) esperadaFinal
  let coritiba = linhaDoTime campeonatoTeste "Coritiba"
      cianorte = linhaDoTime campeonatoTeste "Cianorte"
  testar "desempate: mesmos pontos e vitórias"
    (pontosLinha coritiba == pontosLinha cianorte && vitorias coritiba == vitorias cianorte) True
  testar "desempate decidido pelo saldo (Coritiba +3, Cianorte +1)" (saldo coritiba, saldo cianorte) (3, 1)
  testar "Coritiba fica à frente de Cianorte" (map time (take 2 (drop 2 (classificacao campeonatoTeste))))
    ["Coritiba", "Cianorte"]
  testar "classificacao ateRodada 3" (classificacao (ateRodada 3 campeonatoTeste)) esperadaRodada3
  testar "rodada 3 difere da final"
    (classificacao (ateRodada 3 campeonatoTeste) /= classificacao campeonatoTeste) True
  testar "ateRodada 0" (ateRodada 0 campeonatoTeste) []
  testar "aproveitamento do líder" (aproveitamento (linhaDoTime campeonatoTeste "Athletico")) 80.0
  testar "aproveitamento sem jogos" (aproveitamento (Linha "X" 0 0 0 0 0 0)) 0
  testar "maiorGoleada []" (maiorGoleada []) Nothing
  testar "mediaGols []" (mediaGols []) Nothing
  testar "maiorGoleada do campeonato" (maiorGoleada campeonatoTeste) (Just (Partida 1 "Coritiba" 4 0 "Londrina"))
  testar "mediaGols do campeonato" (mediaGols campeonatoTeste) (Just (32 / 15))
  testar "classificacao []" (classificacao []) []

  putStrLn "Camada 3:"
  testar "separar" (separar ';' "a;b;;c") ["a", "b", "", "c"]
  testar "separar string vazia" (separar ';' "") [""]
  testar "lerCampeonato entrada válida" (lerCampeonato dadosCampeonato) (Right campeonatoTeste)
  testar "lerCampeonato ignora comentário e linha em branco"
    (lerCampeonato "# c\n\n1;A;1;0;B\n") (Right [Partida 1 "A" 1 0 "B"])
  testar "lerCampeonato vazio" (lerCampeonato "") (Right [])
  testar "lerPartida com espaços" (lerPartida 1 " 1 ; A ; 2 ; 1 ; B ") (Right (Partida 1 "A" 2 1 "B"))
  testar "Erro CamposInvalidos" (lerPartida 1 "1;A;2;B") (Left (CamposInvalidos 1 "1;A;2;B"))
  testar "Erro NumeroInvalido (\"dois\")" (lerPartida 2 "1;A;dois;1;B") (Left (NumeroInvalido 2 "dois"))
  testar "Erro NumeroInvalido (\"2a\")" (lerPartida 2 "1;A;2a;1;B") (Left (NumeroInvalido 2 "2a"))
  testar "Erro PlacarNegativo" (lerPartida 3 "1;A;-1;1;B") (Left (PlacarNegativo 3))
  testar "Erro MesmoTime" (lerPartida 4 "1;A;1;1;A") (Left (MesmoTime 4 "A"))
  testar "Erro TimeRepetidoNaRodada" (lerCampeonato "1;A;1;0;B\n1;A;2;2;C") (Left (TimeRepetidoNaRodada 1 "A"))
  testar "lerCampeonato para no primeiro erro" (lerCampeonato dadosComErros) (Left (NumeroInvalido 2 "dois"))
  testar "todosOsErros lista todos" (todosOsErros dadosComErros)
    [ NumeroInvalido 2 "dois"
    , MesmoTime 3 "Paraná"
    , PlacarNegativo 4
    , CamposInvalidos 5 "2;Londrina;1;0"
    , TimeRepetidoNaRodada 2 "Coritiba"
    ]
  testar "todosOsErros em entrada válida" (todosOsErros dadosCampeonato) []
  testar "tabelaFormatada tem cabeçalho + 6 linhas"
    (length (lines (tabelaFormatada (classificacao campeonatoTeste)))) 7
  testar "tabelaFormatada vazia tem só o cabeçalho" (length (lines (tabelaFormatada []))) 1
