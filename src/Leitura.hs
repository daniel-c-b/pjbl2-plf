-- Camada 3

module Leitura
  ( Erro (..)
  , separar
  , lerPartida
  , lerCampeonato
  , todosOsErros
  , descreverErro
  , descreverPartida
  , formatarDecimal
  , tabelaFormatada
  ) where

import Data.Char (isSpace)
import Data.Either (lefts, rights)
import Data.List (dropWhileEnd, nub, sort)
import Data.Maybe (listToMaybe)
import Text.Read (readMaybe)

import Classificacao (pontosLinha, saldo)
import Tipos

-- erro de leitura
data Erro
  = CamposInvalidos Int String
  | NumeroInvalido Int String
  | PlacarNegativo Int
  | MesmoTime Int Time
  | TimeRepetidoNaRodada Int Time
  deriving (Show, Eq)

separar :: Char -> String -> [String]
separar sep s = case break (== sep) s of
  (campo, [])        -> [campo]
  (campo, _ : resto) -> campo : separar sep resto

aparar :: String -> String
aparar = dropWhileEnd isSpace . dropWhile isSpace

-- conversor de numero pra int
numero :: Int -> String -> Either Erro Int
numero n s = maybe (Left (NumeroInvalido n s)) Right (readMaybe s)

lerPartida :: Int -> String -> Either Erro Partida
lerPartida n linha = do
  (sr, m, sgm, sgv, v) <- case map aparar (separar ';' linha) of
    [a, b, c, d, e] | not (null b || null e) -> Right (a, b, c, d, e)
    _                                         -> Left (CamposInvalidos n linha)
  r  <- numero n sr
  gm <- numero n sgm
  gv <- numero n sgv
  if r <= 0 then Left (NumeroInvalido n sr) else Right ()
  if gm < 0 || gv < 0 then Left (PlacarNegativo n) else Right ()
  if m == v then Left (MesmoTime n m) else Right ()
  return (Partida r m gm gv v)

linhasUteis :: String -> [(Int, String)]
linhasUteis = filter (util . snd) . zip [1 ..] . lines
  where
    util l = case dropWhile isSpace l of
      []        -> False
      ('#' : _) -> False
      _         -> True

errosDeRodada :: [Partida] -> [Erro]
errosDeRodada ps =
  [ TimeRepetidoNaRodada r t
  | r <- sort (nub (map rodada ps))
  , let escalados = concatMap (\p -> [mandante p, visitante p]) (filter ((== r) . rodada) ps)
  , t <- nub escalados
  , length (filter (== t) escalados) > 1
  ]

lerCampeonato :: String -> Either Erro [Partida]
lerCampeonato txt = do
  ps <- traverse (uncurry lerPartida) (linhasUteis txt)
  maybe (Right ()) Left (listToMaybe (errosDeRodada ps))
  return ps

todosOsErros :: String -> [Erro]
todosOsErros txt = lefts resultados ++ errosDeRodada (rights resultados)
  where
    resultados = map (uncurry lerPartida) (linhasUteis txt)

descreverErro :: Erro -> String
descreverErro (CamposInvalidos n l) =
  "linha " ++ show n ++ ": esperados 5 campos separados por ';' e nomes de times não vazios: \"" ++ l ++ "\""
descreverErro (NumeroInvalido n c) =
  "linha " ++ show n ++ ": valor numérico inválido: \"" ++ c ++ "\""
descreverErro (PlacarNegativo n) =
  "linha " ++ show n ++ ": placar negativo"
descreverErro (MesmoTime n t) =
  "linha " ++ show n ++ ": o time " ++ t ++ " joga contra si mesmo"
descreverErro (TimeRepetidoNaRodada r t) =
  "rodada " ++ show r ++ ": o time " ++ t ++ " joga mais de uma vez"

-- partida em texto
descreverPartida :: Partida -> String
descreverPartida p =
  "R" ++ show (rodada p) ++ ": " ++ mandante p ++ " " ++ show (golsMandante p)
    ++ " x " ++ show (golsVisitante p) ++ " " ++ visitante p

-- formataçao de numero decimal pra string
formatarDecimal :: Double -> String
formatarDecimal x = show (fromIntegral (round (x * 100) :: Int) / (100 :: Double))

padDireita :: Int -> String -> String
padDireita n s = s ++ replicate (n - length s) ' '

padEsquerda :: Int -> String -> String
padEsquerda n s = replicate (n - length s) ' ' ++ s

-- tabela printavel
tabelaFormatada :: [Linha] -> String
tabelaFormatada ls = unlines (cabecalho : zipWith formatar [1 ..] ls)
  where
    larg = foldr (max . length . time) 4 ls
    celula = padEsquerda 3
    cabecalho = unwords
      [ padEsquerda 3 "#", padDireita larg "Time", celula "P", celula "J"
      , celula "V", celula "E", celula "D", padEsquerda 4 "SG" ]
    formatar :: Int -> Linha -> String
    formatar pos l = unwords
      [ padEsquerda 3 (show pos), padDireita larg (time l)
      , celula (show (pontosLinha l)), celula (show (jogos l))
      , celula (show (vitorias l)), celula (show (empates l)), celula (show (derrotas l))
      , padEsquerda 4 (comSinal (saldo l)) ]
    comSinal :: Int -> String
    comSinal n = if n > 0 then '+' : show n else show n
