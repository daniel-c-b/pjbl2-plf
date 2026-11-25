# Decisões de modelagem e limitações

## Decisões

**Camada 1**
- `Resultado` é tipo soma (`Vitoria | Empate | Derrota`), não `String`/`Int`;
- `resultadoPara` devolve `Maybe Resultado`. "time não jogou" faz parte do tipo;
- `times` usa `sort . nub`, resultando em lista sem repetição e em ordem alfabética.

**Camada 2**
- `linhaDoTime` usa `foldl` para fazer a acumulação, junto do acumulador `acumular`, que recebe a `Linha` atual e uma `Partida`.
- `Linha` guarda contadores brutos (V, E, D, gols). Pontos e saldo são **derivados** (`pontosLinha`, `saldo`), então não há como ficarem inconsistentes com os contadores.
- A ordenação é uma única função composta com `comparing` e `<>`, pontos decrescente, vitórias decrescente, saldo decrescente, gols pró decrescente e nome crescente. O nome no final torna o resultado determinístico.
- `aproveitamento` converte com `fromIntegral` e devolve 0 para time sem jogos (evita divisão por zero).
- `mediaGols` e `maiorGoleada` com listas vazias devolvem `Nothing`.

**Camada 3**
- `lerPartida` usa `do` em `Either`: o primeiro `Left` interrompe. Conversão numérica via `readMaybe` (nunca `read`).
- `lerCampeonato` usa `traverse` e depois checa time repetido na rodada.
- `todosOsErros` reaproveita `lerPartida` linha a linha (`lefts`/`rights`) e junta os erros de rodada das linhas válidas.
- Cada `Erro` carrega a linha (número original no texto, contando comentários e linhas em branco) ou a rodada.
- Núcleo puro (`Main.relatorio`, `relatorioErros`) separado de `main`, a única função com IO. Os dados ficam em `Dados.hs`, não em `getLine`.
- Rodada deve ser > 0; rodada 0 ou negativa é `NumeroInvalido`.

## Limitações conhecidas

- Não se verifica que cada confronto ocorra uma única vez, nem turno e returno. A leitura aceita qualquer conjunto de partidas sem times repetidos na mesma rodada.
- `times` e `errosDeRodada` usam `nub` (O(n²)); adequado para campeonatos pequenos.
- O número de linha em `Erro` é o da linha no texto de entrada. Se o texto for montado por `unlines`, a contagem começa em 1 na primeira linha.
- A saída depende de terminal UTF-8 para exibir acentos.
