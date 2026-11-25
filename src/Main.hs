module Main (main) where

import Data.Maybe (fromMaybe)

import Classificacao
import Dados
import Leitura
import Tipos

secao :: String -> String
secao titulo = "\n=== " ++ titulo ++ " ===\n"

estatisticas :: [Partida] -> String
estatisticas ps = unlines
  [ "Maior goleada: " ++ maybe "n/d" descreverPartida (maiorGoleada ps)
  , "Média de gols por partida: " ++ fromMaybe "n/d" (formatarDecimal <$> mediaGols ps)
  ]

aproveitamentos :: [Linha] -> String
aproveitamentos = unlines . map (\l -> "  " ++ time l ++ ": " ++ formatarDecimal (aproveitamento l) ++ "%")

relatorio :: String -> String
relatorio dados = case lerCampeonato dados of
  Left e   -> "Erro na leitura: " ++ descreverErro e ++ "\n"
  Right ps -> concat
    [ secao "Tabela final"
    , tabelaFormatada (classificacao ps)
    , secao "Aproveitamento (%)"
    , aproveitamentos (classificacao ps)
    , secao "Tabela após a rodada 3"
    , tabelaFormatada (classificacao (ateRodada 3 ps))
    , secao "Estatísticas"
    , estatisticas ps
    ]

relatorioErros :: String -> String
relatorioErros dados = case lerCampeonato dados of
  Right _ -> "Entrada válida.\n"
  Left e  -> unlines
    ( ("Primeiro erro: " ++ descreverErro e)
    : "Todos os erros:"
    : map (("  - " ++) . descreverErro) (todosOsErros dados) )

main :: IO ()
main = putStr $ concat
  [ relatorio dadosCampeonato
  , secao "Entrada com erros"
  , relatorioErros dadosComErros
  , secao "Campeonato vazio"
  , relatorio ""
  ]
