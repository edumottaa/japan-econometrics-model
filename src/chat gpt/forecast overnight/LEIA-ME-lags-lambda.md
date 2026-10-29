# Comparacao do BVAR estrutural original

1. `6- forecast lag12 lambda.R`: estima apenas 12 lags com lambda = 0,10 e gera
   os quatro cenarios e seus graficos. Reutiliza a base congelada e as condicoes
   do forecast existente de 12 lags. Nao atualiza dados, nao roda CV e nao usa
   tendencia adicional, prior de inflacao ou condicao nova para o juro.
2. `7- comparar lags e lambda.R`: compara os resultados com os forecasts ja
   existentes de seis e 12 lags, ambos com lambda = 0,20.

Seis versus 12 lags com o mesmo lambda isola a mudanca de defasagens. Comparar
12 lags com lambda = 0,20 versus 0,10 isola a mudanca de regularizacao. Comparar
seis lags/0,20 diretamente com 12 lags/0,10 mistura os dois efeitos.

Mu e delta permanecem em 1. O modelo tem sete variaveis e a mesma identificacao
estrutural. A taxa curta continua com overnight de fim de mes a partir de marco
de 2024, conforme a versao de referencia. O horizonte segue ate agosto de 2028.

Menor lambda reforca a contracao para o prior Minnesota. Nao fixa o sentido das
previsoes e nao impoe alta da inflacao ou dos juros.

Saida: `src/Outputs/Forecast BVAR estrutural/overnight fim de mes lag12 lambda010`.

## Resultado da execucao

Concluido em 25/10/2026. Posterior de 2.500 sorteios, ESS 88,9 e 445 conjuntos
distintos de coeficientes. As quatro condicoes de cenario foram respeitadas
com erro numerico zero; os forecasts sao finitos. Sem CV novo.

No cenario base, em agosto/2028:

| Variavel | 6 lags / 0,20 | 12 lags / 0,20 | 12 lags / 0,10 |
|---|---:|---:|---:|
| Atividade, 12 meses (%) | -4,96 | -4,39 | -4,15 |
| CPI core, 12 meses (%) | 1,41 | 1,26 | 1,38 |
| Juro curto (% a.a.) | 1,53 | 1,43 | 1,41 |
| 10a - 2a (p.p.) | 1,39 | 1,41 | 1,37 |
| USD/JPY | 162,99 | 158,22 | 160,36 |

Nesta amostra, 12 lags nao eliminaram a desinflacao ou a queda posterior do
juro. Lambda 0,10 elevou um pouco a inflacao em relacao a 12 lags/0,20, mas
reduziu o juro previsto. Sao comparacoes das medias, nao evidencias de melhor
acuracia fora da amostra. Atividade e o indice coincidente, nao PIB.

`comparacao_lags_lambda.png` inclui a historia desde 2019;
`comparacao_forecast.png` amplia o periodo previsto. As demais figuras mostram
cada variavel e os quatro cenarios. `comparacao_marcos.csv` contem os numeros.
