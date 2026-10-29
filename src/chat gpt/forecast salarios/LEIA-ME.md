# BVAR estrutural com salarios

1. `src/1- data_collection/2- salarios.R`: coleta MHLW/e-Stat; tambem chamado pela coleta principal.
2. `2- forecast bvar salarios.R`: prepara a base, estima e gera os graficos.
3. `3- graficos forecast salarios.R`: refaz os graficos a partir do forecast salvo.

O modelo de referencia e o BVAR original com overnight de fim de mes: seis lags,
lambda 0,20, mu e delta 1. A unica serie acrescentada e salario. Mantem os
320 meses de janeiro/2000 a agosto/2026 e os mesmos quatro caminhos de Brent
e Treasury. Nao inclui tendencia adicional, prior de inflacao ou condicao para o juro.

Salario e a variacao oficial em 12 meses (%) dos salarios regulares nominais,
todos os empregados e setores cobertos, estabelecimentos com cinco ou mais empregados.
Exclui bonus e horas extras. A variacao oficial evita calcular crescimento a
partir de niveis do indice afetados pelas mudancas de base da pesquisa.
Ja entra em %, sem log, diferenca ou ajuste sazonal adicional. O futuro e endogeno:
o Shunto nao e imposto como trajetoria. A resposta dos salarios aos choques e
o choque adicional ficam sem novas restricoes de sinal.

Fonte: https://www.e-stat.go.jp/stat-search/files?layout=dataset&stat_infid=000032189732
Nota de revisao: https://www.mhlw.go.jp/toukei/list/dl/maikin-kaisetsu-20240408.pdf

Modelo salvo em `data/processed/forecast_bvar_salarios`.
Resultados em `src/Outputs/Forecast BVAR estrutural/salarios`.
Forecast exploratorio, sem CV novo. A inclusao nao obriga inflacao ou juros a subir.

## Resultado

Execucao concluida em 25/10/2026: 2.500 sorteios, ESS 467 e 666 conjuntos distintos
de coeficientes. Condicoes dos quatro cenarios respeitadas, resultados finitos e
mesmas sete series do modelo original. Foram geradas 17 imagens.

Medias do cenario base:

| Variavel | Dez/2026 | Dez/2027 | Ago/2028 |
|---|---:|---:|---:|
| CPI core, 12 meses (%) | 2,11 | 1,81 | 1,55 |
| Juro curto (% a.a.) | 1,57 | 2,11 | 2,23 |
| Salarios, 12 meses (%) | 3,75 | 3,43 | 3,34 |
| Atividade, 12 meses (%) | 4,68 | -6,32 | -5,67 |
| JGB 10a - 2a (p.p.) | 1,17 | 1,39 | 1,59 |
| USD/JPY | 153,63 | 155,40 | 154,11 |

O juro sobe, mas a inflacao ainda desacelera e a atividade permanece negativa
no fim do horizonte. Atividade e o indice coincidente, nao PIB. Estes resultados
nao incluem o prior adicional de longo prazo, tendencia ou reajustes Shunto impostos.
